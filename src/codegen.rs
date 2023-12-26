use crate::{
    ast::{LiteralValue, Node},
    lexer::{Token, TokenType},
    parser::Symbol,
    types::Type,
};

pub struct Assembly {
    pub data: String,
    pub text: String,
}
impl Assembly {
    fn new() -> Assembly {
        Assembly {
            data: String::new(),
            text: String::new(),
        }
    }
}

pub struct CodeGen {
    nodes: Vec<Node>,
    assembly: Assembly,
    registers: [bool; 4],
    label_count: usize,
    assignment_depth: usize,
}

const REGISTER_NAMES: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];
const BYTE_REGISTER_NAMES: [&str; 4] = ["%r8b", "%r9b", "%r10b", "%r11b"];
const WORD_REGISTER_NAMES: [&str; 4] = ["%r8w", "%r9w", "%r10w", "%r11w"];
const DWORD_REGISTER_NAMES: [&str; 4] = ["%r8d", "%r9d", "%r10d", "%r11d"];

impl CodeGen {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self {
            nodes,
            assembly: Assembly::new(),
            registers: [false; 4],
            label_count: 0,
            assignment_depth: 0,
        }
    }

    pub fn generate(&mut self) -> String {
        self.preamble();

        for node in self.nodes.clone() {
            self.generate_node(node);
        }

        // combine the data and text sections
        let mut assembly = String::new();
        assembly.push_str(&self.assembly.data);
        assembly.push_str(&self.assembly.text);

        assembly
    }

    fn generate_node(&mut self, node: Node) -> usize {
        match node {
            Node::LiteralExpr { value, ty } => match value {
                LiteralValue::U8(u) => self.load(u as u64, ty),
                LiteralValue::U16(u) => self.load(u as u64, ty),
                LiteralValue::U32(u) => self.load(u as u64, ty),
                LiteralValue::U64(u) => self.load(u as u64, ty),
                LiteralValue::Identifier(i) => self.load_global(i, ty),
            },
            Node::BinaryExpr {
                left,
                operator,
                right,
                ..
            } => {
                let left = self.generate_node(*left);
                let right = self.generate_node(*right);

                match operator.token_type {
                    TokenType::Add => self.add(left, right),
                    TokenType::Sub => self.subtract(left, right),
                    TokenType::Mul => self.multiply(left, right),
                    TokenType::Div => self.divide(left, right),
                    TokenType::Equal
                    | TokenType::NotEqual
                    | TokenType::LessThan
                    | TokenType::LessThanOrEqual
                    | TokenType::GreaterThan
                    | TokenType::GreaterThanOrEqual => {
                        self.compare_and_set(operator.token_type, left, right)
                    }
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
            Node::UnaryExpr {
                operator,
                right,
                ty,
            } => {
                match operator.token_type {
                    TokenType::Sub => {
                        let right_node = self.generate_node(*right.clone());
                        self.load(0, ty);
                        self.subtract(0, right_node)
                    }
                    TokenType::Ampersand => {
                        // get identifier
                        let identifier = match &*right {
                            Node::LiteralExpr { value, .. } => match value {
                                LiteralValue::Identifier(i) => i,
                                _ => panic!("Unexpected token {:?}", right),
                            },
                            _ => panic!("Unexpected token {:?}", right),
                        };

                        self.address_of(identifier.to_string())
                    }
                    TokenType::Mul => {
                        let right_node = self.generate_node(*right.clone());
                        self.dereference(right_node, right.ty().unwrap())
                    }
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
            Node::ScaleExpr { right, size, ty } => {
                let right_node = self.generate_node(*right.clone());
                let shift_by = match size {
                    2 => 1,
                    4 => 2,
                    8 => 3,
                    _ => panic!("Unexpected type {:?}", ty),
                };
                self.scale(right_node, shift_by)
            }
            Node::WidenExpr { right, ty } => {
                let right_node = self.generate_node(*right.clone());
                self.widen(right_node, right.ty().unwrap(), ty)
            }
            Node::GlobalVar { identifier, ty } => {
                self.define_global(identifier.lexeme.unwrap(), ty);
                0
            }
            Node::GlobalVarMany { identifiers, ty } => {
                for identifier in identifiers {
                    self.define_global(identifier.lexeme.unwrap(), ty.clone());
                }
                0
            }
            Node::AssignStmt { left, expr } => {
                self.assignment_depth += 1;
                let r = match *left.clone() {
                    Node::LiteralExpr { value, .. } => match value {
                        LiteralValue::Identifier(i) => {
                            let register = self.generate_node(*expr.clone());
                            self.store(register, i, expr.ty().unwrap());
                            self.assignment_depth -= 1;
                            register
                        }
                        _ => panic!("Unexpected token {:?}", left),
                    },
                    Node::UnaryExpr {
                        operator,
                        right,
                        ty,
                    } => match operator.token_type {
                        TokenType::Mul => {
                            let right_node = self.generate_node(*right.clone());
                            let expr_node = self.generate_node(*expr.clone());
                            self.store_dereference(expr_node, right_node, ty);
                            self.free_register(expr_node);
                            self.assignment_depth -= 1;
                            right_node
                        }
                        _ => panic!("Unexpected token {:?}", left),
                    },
                    _ => panic!("Unexpected token {:?}", left),
                };

                if self.assignment_depth == 0 {
                    self.free_register(r);
                }

                r
            }
            Node::IfStmt {
                condition,
                then_branch,
                else_branch,
            } => self.if_stmt(condition, then_branch, else_branch),
            Node::CompoundStmt { statements } => {
                for statement in statements {
                    self.generate_node(statement);
                }
                0
            }
            Node::WhileStmt { condition, body } => self.while_stmt(condition, body),
            Node::FnDecl {
                identifier, body, ..
            } => self.function(identifier, body),
            Node::FnCall {
                identifier, expr, ..
            } => {
                // TODO: fix ths hack
                let r = self.function_call(identifier.clone(), expr);
                if identifier.lexeme.unwrap() == "printint" {
                    self.free_register(r);
                    0
                } else {
                    r
                }
            }
            Node::ReturnStmt { expr, fn_name } => self.return_stmt(expr, fn_name),
        }
    }

    fn preamble(&mut self) {
        self.free_all_registers();
        self.assembly.text.push_str("\t.text\n");
        self.assembly.text.push_str(".LC0:\n");
        self.assembly.text.push_str("\t.string\t\"%d\\n\"\n");
        self.assembly.text.push_str("printint:\n");
        self.assembly.text.push_str("\tpushq\t%rbp\n");
        self.assembly.text.push_str("\tmovq\t%rsp, %rbp\n");
        self.assembly.text.push_str("\tsubq\t$16, %rsp\n");
        self.assembly.text.push_str("\tmovl\t%edi, -4(%rbp)\n");
        self.assembly.text.push_str("\tmovl\t-4(%rbp), %eax\n");
        self.assembly.text.push_str("\tmovl\t%eax, %esi\n");
        self.assembly.text.push_str("\tleaq\t.LC0(%rip), %rdi\n");
        self.assembly.text.push_str("\tmovl\t$0, %eax\n");
        self.assembly.text.push_str("\tcall\tprintf@PLT\n");
        self.assembly.text.push_str("\tnop\n");
        self.assembly.text.push_str("\tleave\n");
        self.assembly.text.push_str("\tret\n\n");
    }

    fn load(&mut self, value: u64, _ty: Type) -> usize {
        let r = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t${}, {}\n", value, REGISTER_NAMES[r]));
        r
    }

    fn load_global(&mut self, identifier: String, ty: Type) -> usize {
        let r = self.allocate_register();
        if ty == Type::U8 {
            self.assembly.text.push_str(&format!(
                "\tmovzbq\t{}, {}\n",
                identifier, REGISTER_NAMES[r]
            ));
        } else if ty == Type::U16 {
            self.assembly
                .text
                .push_str(&format!("\tmovzx\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if ty == Type::U32 {
            self.assembly
                .text
                .push_str(&format!("\tmovq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if ty == Type::U64
            || ty == Type::PU8
            || ty == Type::PU16
            || ty == Type::PU32
            || ty == Type::PU64
        {
            self.assembly
                .text
                .push_str(&format!("\tmovq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if let Type::Array { ty, count } = ty {
            self.assembly
                .text
                .push_str(&format!("\tleaq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else {
            panic!("Unexpected type {:?}", ty);
        }

        r
    }

    fn store(&mut self, register: usize, identifier: String, ty: Type) {
        let ty = match ty {
            Type::Array { ty, .. } => ty.pointer_to(),
            _ => ty,
        };
        if ty == Type::U8 {
            self.assembly.text.push_str(&format!(
                "\tmovb\t{}, {}\n",
                BYTE_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::U16 {
            self.assembly.text.push_str(&format!(
                "\tmovw\t{}, {}\n",
                WORD_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::U32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}, {}\n",
                DWORD_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::U64
            || ty == Type::PU8
            || ty == Type::PU16
            || ty == Type::PU32
            || ty == Type::PU64
        {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}, {}\n",
                REGISTER_NAMES[register], identifier
            ));
        } else {
            panic!("Unexpected type {:?}", ty);
        }
    }

    fn widen(&mut self, register: usize, _old_ty: Type, _new_ty: Type) -> usize {
        register
    }

    fn define_global(&mut self, identifier: String, ty: Type) {
        let type_size = ty.size();

        self.assembly
            .data
            .push_str(&format!("\t.data\n\t.global\t{}\n", identifier));
        self.assembly.data.push_str(&format!("{}:\n", identifier));

        let count = match ty {
            Type::Array { count, .. } => count,
            _ => 1,
        };

        let size_str = match type_size {
            1 => "\t.byte\t0\n",
            2 => "\t.short\t0\n",
            4 => "\t.long\t0\n",
            8 => "\t.quad\t0\n",
            _ => panic!("Unexpected size {}", type_size),
        };

        for _ in 0..count {
            self.assembly.data.push_str(size_str);
        }
    }

    fn add(&mut self, left: usize, right: usize) -> usize {
        self.assembly.text.push_str(&format!(
            "\taddq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn subtract(&mut self, left: usize, right: usize) -> usize {
        self.assembly.text.push_str(&format!(
            "\tsubq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.free_register(right);
        left
    }

    fn multiply(&mut self, left: usize, right: usize) -> usize {
        self.assembly.text.push_str(&format!(
            "\timulq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn divide(&mut self, left: usize, right: usize) -> usize {
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, %rax\n", REGISTER_NAMES[left]));
        self.assembly.text.push_str("\tcqo\n");
        self.assembly
            .text
            .push_str(&format!("\tidivq\t{}\n", REGISTER_NAMES[right]));
        self.assembly
            .text
            .push_str(&format!("\tmovq\t%rax, {}\n", REGISTER_NAMES[left]));
        self.free_register(right);
        left
    }

    fn allocate_register(&mut self) -> usize {
        for (i, available) in self.registers.iter_mut().enumerate() {
            if !*available {
                *available = true;
                return i;
            }
        }

        panic!("No available register");
    }

    fn free_register(&mut self, register: usize) {
        self.registers[register] = false;
    }

    fn free_all_registers(&mut self) {
        for i in 0..self.registers.len() {
            self.free_register(i);
        }
    }

    fn compare_and_jump(&mut self, operation: TokenType, left: usize, right: usize, label: usize) {
        // get inverted jump instructions
        let jump_instruction = match operation {
            TokenType::Equal => "jne",
            TokenType::NotEqual => "je",
            TokenType::LessThan => "jge",
            TokenType::LessThanOrEqual => "jg",
            TokenType::GreaterThan => "jle",
            TokenType::GreaterThanOrEqual => "jl",
            _ => panic!("Unexpected token {:?}", operation),
        };

        self.assembly.text.push_str(&format!(
            "\tcmpq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.assembly
            .text
            .push_str(&format!("\t{} L{}\n", jump_instruction, label));
        self.free_all_registers();
    }

    fn compare_and_set(&mut self, operation: TokenType, left: usize, right: usize) -> usize {
        // get set instructions
        let set_instruction = match operation {
            TokenType::Equal => "sete",
            TokenType::NotEqual => "setne",
            TokenType::LessThan => "setl",
            TokenType::LessThanOrEqual => "setle",
            TokenType::GreaterThan => "setg",
            TokenType::GreaterThanOrEqual => "setge",
            _ => panic!("Unexpected token {:?}", operation),
        };

        self.assembly.text.push_str(&format!(
            "\tcmpq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.assembly.text.push_str(&format!(
            "\t{} {}\n",
            set_instruction, BYTE_REGISTER_NAMES[right]
        ));
        self.assembly.text.push_str(&format!(
            "\tmovzbq\t{}, {}\n",
            BYTE_REGISTER_NAMES[right], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn label(&mut self) -> usize {
        self.label_count += 1;
        self.label_count
    }

    fn generate_label(&mut self, label: usize) {
        self.assembly.text.push_str(&format!("L{}:\n", label));
    }

    fn jump(&mut self, label: usize) {
        self.assembly.text.push_str(&format!("\tjmp\tL{}\n", label));
    }

    fn if_stmt(
        &mut self,
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
    ) -> usize {
        let false_label = self.label();
        let end_label = self.label();

        let (left_reg, right_reg, operation) = match *condition {
            Node::BinaryExpr {
                left,
                operator,
                right,
                ..
            } => {
                let left_reg = self.generate_node(*left);
                let right_reg = self.generate_node(*right);

                (left_reg, right_reg, operator.token_type)
            }
            _ => panic!("Unexpected token {:?}", condition),
        };

        // zero jump to the false label
        self.compare_and_jump(operation, left_reg, right_reg, false_label);
        self.free_all_registers();

        // generate the then branch code
        self.generate_node(*then_branch);
        self.free_all_registers();
        // unconditional jump to the end label
        self.jump(end_label);

        // generate the false label
        self.generate_label(false_label);

        // generate the else branch code
        if let Some(else_branch) = else_branch {
            self.generate_node(*else_branch);
            self.free_all_registers();
        }

        // generate the end label
        self.generate_label(end_label);
        0
    }

    fn while_stmt(&mut self, condition: Box<Node>, body: Box<Node>) -> usize {
        let start_label = self.label();
        let end_label = self.label();

        self.generate_label(start_label);

        let (left_reg, right_reg, operation) = match *condition {
            Node::BinaryExpr {
                left,
                operator,
                right,
                ..
            } => {
                let left_reg = self.generate_node(*left);
                let right_reg = self.generate_node(*right);

                (left_reg, right_reg, operator.token_type)
            }
            _ => panic!("Unexpected token {:?}", condition),
        };

        // zero jump to the end label
        self.compare_and_jump(operation, left_reg, right_reg, end_label);
        self.free_all_registers();

        // generate the body code
        self.generate_node(*body);
        self.free_all_registers();

        // unconditional jump to the start label
        self.jump(start_label);

        // generate the end label
        self.generate_label(end_label);
        0
    }

    fn function(&mut self, identifier: Token, body: Box<Node>) -> usize {
        let fn_name = identifier.lexeme.unwrap();
        self.function_preamble(fn_name.clone());
        self.generate_node(*body);
        self.function_postamble(fn_name.clone());
        0
    }

    fn function_preamble(&mut self, name: String) {
        self.assembly.text.push_str("\t.global main\n");
        self.assembly
            .text
            .push_str(&format!("\t.type\t{}, @function\n", name));
        self.assembly.text.push_str(&format!("{}:\n", name));
        self.assembly.text.push_str("\tpushq\t%rbp\n");
        self.assembly.text.push_str("\tmovq\t%rsp, %rbp\n");
    }

    fn function_postamble(&mut self, fn_name: String) {
        // self.assembly.text.push_str(&format!("\tmovl\t$0, %eax\n"));
        // self.assembly.text.push_str(&format!("\tpopq\t%rbp\n"));
        // self.assembly.text.push_str(&format!("\tret\n"));
        self.assembly
            .text
            .push_str(format!("{}_end:\n", fn_name).as_str());
        self.assembly.text.push_str(&format!("\tpopq\t%rbp\n"));
        self.assembly.text.push_str(&format!("\tret\n"));
    }

    fn function_call(&mut self, identifier: crate::lexer::Token, expr: Box<Node>) -> usize {
        let register = self.generate_node(*expr);
        let out_register = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, %rdi\n", REGISTER_NAMES[register]));
        self.assembly
            .text
            .push_str(&format!("\tcall\t{}\n", identifier.lexeme.unwrap()));
        self.assembly
            .text
            .push_str(&format!("\tmovq\t%rax, {}\n", REGISTER_NAMES[out_register]));
        self.free_register(register);
        out_register
    }

    fn return_stmt(&mut self, expr: Box<Node>, fn_name: Symbol) -> usize {
        let register = self.generate_node(*expr);
        match fn_name.ty.clone().unwrap() {
            Type::U8 => {
                self.assembly.text.push_str(&format!(
                    "\tmovzbl\t{}, %eax\n",
                    BYTE_REGISTER_NAMES[register]
                ));
            }
            Type::U16 => {
                self.assembly.text.push_str(&format!(
                    "\tmovzwl\t{}, %eax\n",
                    WORD_REGISTER_NAMES[register]
                ));
            }
            Type::U32 => {
                self.assembly.text.push_str(&format!(
                    "\tmovl\t{}, %eax\n",
                    DWORD_REGISTER_NAMES[register]
                ));
            }
            Type::U64 | Type::PU8 | Type::PU16 | Type::PU32 | Type::PU64 => {
                self.assembly
                    .text
                    .push_str(&format!("\tmovq\t{}, %rax\n", REGISTER_NAMES[register]));
            }
            _ => panic!("Unexpected type {:?}", fn_name.ty.clone().unwrap()),
        }
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, %rax\n", REGISTER_NAMES[register]));
        self.free_register(register);
        0
    }

    fn address_of(&mut self, ident: String) -> usize {
        let r = self.allocate_register();

        self.assembly
            .text
            .push_str(&format!("\tleaq\t{}(%rip), {}\n", ident, REGISTER_NAMES[r]));

        r
    }

    fn dereference(&mut self, register: usize, ty: Type) -> usize {
        let ty = match ty {
            Type::Array { ty, .. } => ty.pointer_to(),
            _ => ty,
        };
        match ty {
            Type::PU8 => self.assembly.text.push_str(&format!(
                "\tmovzbq\t({}), {}\n",
                REGISTER_NAMES[register], REGISTER_NAMES[register]
            )),
            Type::PU16 => self.assembly.text.push_str(&format!(
                "\tmovzx\t({}), {}\n",
                REGISTER_NAMES[register], REGISTER_NAMES[register]
            )),
            Type::PU32 => self.assembly.text.push_str(&format!(
                "\tmovq\t({}), {}\n",
                REGISTER_NAMES[register], REGISTER_NAMES[register]
            )),
            Type::PU64 => self.assembly.text.push_str(&format!(
                "\tmovq\t({}), {}\n",
                REGISTER_NAMES[register], REGISTER_NAMES[register]
            )),
            _ => panic!("Unexpected type {:?}", ty),
        }

        register
    }

    fn scale(&mut self, register: usize, value: u8) -> usize {
        self.assembly.text.push_str(&format!(
            "\tsalq\t${}, {}\n",
            value, REGISTER_NAMES[register]
        ));
        register
    }

    fn store_dereference(&mut self, expr_node: usize, right_node: usize, ty: Type) -> usize {
        let ty = match ty {
            Type::Array { ty, .. } => ty.pointer_to(),
            _ => ty,
        };

        match ty {
            Type::PU8 => self.assembly.text.push_str(&format!(
                "\tmovb\t{}, ({})\n",
                BYTE_REGISTER_NAMES[expr_node], REGISTER_NAMES[right_node]
            )),
            Type::PU16 => self.assembly.text.push_str(&format!(
                "\tmovw\t{}, ({})\n",
                WORD_REGISTER_NAMES[expr_node], REGISTER_NAMES[right_node]
            )),
            Type::PU32 => self.assembly.text.push_str(&format!(
                "\tmovl\t{}, ({})\n",
                DWORD_REGISTER_NAMES[expr_node], REGISTER_NAMES[right_node]
            )),
            Type::PU64 => self.assembly.text.push_str(&format!(
                "\tmovq\t{}, ({})\n",
                REGISTER_NAMES[expr_node], REGISTER_NAMES[right_node]
            )),
            _ => panic!("Unexpected type {:?}", ty),
        };
        expr_node
    }
}
