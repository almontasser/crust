use crate::{
    ast::Node,
    lexer::{Literal, TokenType},
};

pub struct CodeGen {
    nodes: Vec<Node>,
    assembly: String,
    registers: [bool; 4],
    label_count: usize,
}

const REGISTER_NAMES: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];
const BYTE_REGISTER_NAMES: [&str; 4] = ["%r8b", "%r9b", "%r10b", "%r11b"];

impl CodeGen {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self {
            nodes,
            assembly: String::new(),
            registers: [false; 4],
            label_count: 0,
        }
    }

    pub fn generate(&mut self) -> String {
        self.preamble();

        for node in self.nodes.clone() {
            self.generate_node(node);
        }

        self.postamble();

        self.assembly.clone()
    }

    fn generate_node(&mut self, node: Node) -> usize {
        match node {
            Node::LiteralExpr { value } => match value {
                Literal::Integer(i) => self.load(i as i64),
                Literal::Identifier(i) => self.load_global(i),
            },
            Node::BinaryExpr {
                left,
                operator,
                right,
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
            Node::UnaryExpr { operator, right } => {
                let right = self.generate_node(*right);

                match operator.token_type {
                    TokenType::Sub => {
                        self.load(0);
                        self.subtract(0, right)
                    }
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
            Node::PrintStmt { expr } => {
                let register = self.generate_node(*expr);
                self.printint(register);
                0
            }
            Node::GlobalVar { identifier } => {
                self.define_global(identifier.lexeme.unwrap());
                0
            }
            Node::AssignStmt { identifier, expr } => {
                let register = self.generate_node(*expr);
                self.store(register, identifier.lexeme.unwrap());
                self.free_register(register);
                0
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
        }
    }

    fn preamble(&mut self) {
        self.free_all_registers();
        self.assembly.push_str("\t.text\n");
        self.assembly.push_str(".LC0:\n");
        self.assembly.push_str(".string\t\"%d\\n\"\n");
        self.assembly.push_str("printint:\n");
        self.assembly.push_str("\tpushq\t%rbp\n");
        self.assembly.push_str("\tmovq\t%rsp, %rbp\n");
        self.assembly.push_str("\tsubq\t$16, %rsp\n");
        self.assembly.push_str("\tmovl\t%edi, -4(%rbp)\n");
        self.assembly.push_str("\tmovl\t-4(%rbp), %eax\n");
        self.assembly.push_str("\tmovl\t%eax, %esi\n");
        self.assembly.push_str("\tleaq\t.LC0(%rip), %rdi\n");
        self.assembly.push_str("\tmovl\t$0, %eax\n");
        self.assembly.push_str("\tcall\tprintf@PLT\n");
        self.assembly.push_str("\tnop\n");
        self.assembly.push_str("\tleave\n");
        self.assembly.push_str("\tret\n\n");
        self.assembly.push_str("\t.global main\n");
        self.assembly.push_str("\t.type\tmain, @function\n");
        self.assembly.push_str("main:\n");
        self.assembly.push_str("\tpushq\t%rbp\n");
        self.assembly.push_str("\tmovq\t%rsp, %rbp\n");
    }

    fn postamble(&mut self) {
        self.assembly.push_str("\tmovl\t$0, %eax\n");
        self.assembly.push_str("\tpopq\t%rbp\n");
        self.assembly.push_str("\tret\n");
    }

    fn load(&mut self, value: i64) -> usize {
        let r = self.allocate_register();
        self.assembly
            .push_str(&format!("\tmovq\t${}, {}\n", value, REGISTER_NAMES[r]));
        r
    }

    fn load_global(&mut self, identifier: String) -> usize {
        let r = self.allocate_register();
        self.assembly.push_str(&format!(
            "\tmovq\t{}(%rip), {}\n",
            identifier, REGISTER_NAMES[r]
        ));
        r
    }

    fn store(&mut self, register: usize, identifier: String) {
        self.assembly.push_str(&format!(
            "\tmovq\t{}, {}(%rip)\n",
            REGISTER_NAMES[register], identifier
        ));
    }

    fn define_global(&mut self, identifier: String) {
        self.assembly
            .push_str(&format!("\t.comm\t{},8,8\n", identifier));
    }

    fn add(&mut self, left: usize, right: usize) -> usize {
        self.assembly.push_str(&format!(
            "\taddq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn subtract(&mut self, left: usize, right: usize) -> usize {
        self.assembly.push_str(&format!(
            "\tsubq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.free_register(right);
        left
    }

    fn multiply(&mut self, left: usize, right: usize) -> usize {
        self.assembly.push_str(&format!(
            "\timulq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn divide(&mut self, left: usize, right: usize) -> usize {
        self.assembly
            .push_str(&format!("\tmovq\t{}, %rax\n", REGISTER_NAMES[left]));
        self.assembly.push_str("\tcqo\n");
        self.assembly
            .push_str(&format!("\tidivq\t{}\n", REGISTER_NAMES[right]));
        self.assembly
            .push_str(&format!("\tmovq\t%rax, {}\n", REGISTER_NAMES[left]));
        self.free_register(right);
        left
    }

    fn printint(&mut self, register: usize) {
        self.assembly
            .push_str(&format!("\tmovq\t{}, %rdi\n", REGISTER_NAMES[register]));
        self.assembly.push_str("\tcall\tprintint\n");
        self.free_register(register);
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

        self.assembly.push_str(&format!(
            "\tcmpq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.assembly
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

        self.assembly.push_str(&format!(
            "\tcmpq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.assembly.push_str(&format!(
            "\t{} {}\n",
            set_instruction, BYTE_REGISTER_NAMES[right]
        ));
        self.assembly.push_str(&format!(
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
        self.assembly.push_str(&format!("L{}:\n", label));
    }

    fn jump(&mut self, label: usize) {
        self.assembly.push_str(&format!("\tjmp\tL{}\n", label));
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
}
