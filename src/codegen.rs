use crate::{
    ast::{LiteralValue, Node},
    lexer::{Token, TokenType},
    parser::{StorageClass, Symbol},
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
    registers: [bool; 10],
    label_count: usize,
    assignment_depth: usize,
    local_offset: isize,
    stack_offset: isize,
}

const FIRST_PARAM_REG: isize = 9;

const REGISTER_NAMES: [&str; 10] = [
    "%r10", "%r11", "%r12", "%r13", "%r9", "%r8", "%rcx", "%rdx", "%rsi", "%rdi",
];
const BYTE_REGISTER_NAMES: [&str; 10] = [
    "%r10b", "%r11b", "%r12b", "%r13b", "%r9b", "%r8b", "%cl", "%dl", "%sil", "%dil",
];
const WORD_REGISTER_NAMES: [&str; 10] = [
    "%r10w", "%r11w", "%r12w", "%r13w", "%r9w", "%r8w", "%cx", "%dx", "%si", "%di",
];
const DWORD_REGISTER_NAMES: [&str; 10] = [
    "%r10d", "%r11d", "%r12d", "%r13d", "%r9d", "%r8d", "%ecx", "%edx", "%esi", "%edi",
];

impl CodeGen {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self {
            nodes,
            assembly: Assembly::new(),
            registers: [false; 10],
            label_count: 0,
            assignment_depth: 0,
            local_offset: 0,
            stack_offset: 0,
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
                LiteralValue::U64(u) => self.load(u, ty),
                LiteralValue::Identifier(s) => match s.borrow().class {
                    StorageClass::Local | StorageClass::Param => {
                        self.load_local(s.borrow().to_owned(), ty)
                    }
                    StorageClass::Global => self.load_global(s.borrow().clone(), ty),
                },
                LiteralValue::String { value: s, label } => {
                    self.define_string(label.clone(), s);
                    self.load_string(label)
                }
            },
            Node::BinaryExpr {
                left,
                operator,
                right,
                ty,
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
                    TokenType::LogicalAnd => self.logical_and(left, right, ty),
                    TokenType::LogicalOr => self.logical_or(left, right, ty),
                    TokenType::Ampersand => self.bitwise_and(left, right, ty),
                    TokenType::Or => self.bitwise_or(left, right, ty),
                    TokenType::Xor => self.bitwise_xor(left, right, ty),
                    TokenType::LeftShift => self.shift_left(left, right, ty),
                    TokenType::RightShift => self.shift_right(left, right, ty),
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
            Node::UnaryExpr {
                operator, right, ..
            } => {
                match operator.token_type {
                    TokenType::Sub => {
                        let right_node = self.generate_node(*right.clone());
                        self.negate(right_node)
                    }
                    TokenType::Ampersand => {
                        // If & is applied to an expression, we want the address of that expression.
                        // generate_node_address is designed for this.
                        // It can handle identifiers, struct field accesses, and dereferences (*ptr).
                        self.generate_node_address(*right)
                    }
                    TokenType::Mul => {
                        // This is dereferencing a pointer's value, e.g. *p
                        // right_node will hold the address from evaluating p
                        let right_node_val_reg = self.generate_node(*right.clone());
                        // ty_ptr is the type of p (e.g. *i32)
                        let ty_ptr = right.ty().unwrap_or_else(|| panic!("Dereferenced expression has no type"));
                        self.dereference_pointer_value(right_node_val_reg, ty_ptr)
                    }
                    TokenType::Invert => {
                        let right_node = self.generate_node(*right.clone());
                        self.invert(right_node)
                    }
                    TokenType::LogicalNot => {
                        let right_node = self.generate_node(*right.clone());
                        self.logical_not(right_node)
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
            Node::VarDecl {
                symbol,
                is_local,
                ty,
            } => {
                if !is_local {
                    self.define_global(
                        symbol
                            .borrow()
                            .identifier
                            .lexeme
                            .as_ref()
                            .unwrap()
                            .to_owned(),
                        ty,
                    );
                }
                0
            }
            Node::VarDeclMany {
                symbols,
                is_local,
                ty,
            } => {
                for symbol in symbols {
                    if !is_local {
                        self.define_global(
                            symbol
                                .borrow()
                                .identifier
                                .lexeme
                                .as_ref()
                                .unwrap()
                                .to_owned(),
                            ty.clone(),
                        );
                    }
                }
                0
            }
            Node::AssignStmt { left, expr } => {
                self.assignment_depth += 1;
                let r = match *left.clone() {
                    Node::LiteralExpr {
                        value: LiteralValue::Identifier(s),
                        ..
                    } => {
                        let value_reg = self.generate_node(*expr.clone());
                        self.store( // store handles symbol storage (global/local variable)
                            value_reg,
                            s.borrow().clone(),
                            s.borrow().ty.as_ref().unwrap().to_owned(),
                        );
                        self.assignment_depth -= 1;
                        value_reg // Assignment evaluates to the stored value
                    }
                    Node::UnaryExpr { operator, right, ty } if operator.token_type == TokenType::Mul => {
                        // Assignment to *address, e.g. *p = val
                        // `right` (Node) is `p`. `ty` (Type) is the type `*p` points to (e.g., i32 if p is *i32).
                        // `expr` (Node) is `val`.
                        let address_reg = self.generate_node(*right.clone()); // Evaluate `p` to get its value (the address)
                        let value_reg = self.generate_node(*expr.clone());   // Evaluate `val`
                        // ty is the type of the data at the address (e.g. i32), used by store_memory
                        self.store_memory(value_reg, address_reg, ty);
                        self.free_register(address_reg); // Address register can be freed after store
                        self.assignment_depth -= 1;
                        value_reg // Assignment evaluates to the stored value
                    }
                    Node::StructFieldAccess { object, field, ty } => {
                        // Assignment to struct field, e.g. mystruct.myfield = val
                        // `object` (Node) is `mystruct`. `field` (Token) is `myfield`.
                        // `ty` (Type) is the type of the field `myfield`.
                        // `expr` (Node) is `val`.

                        // 1. Get address of the field: &mystruct.myfield
                        let field_node_for_addr = Node::StructFieldAccess {
                             object: object.clone(),
                             field: field.clone(),
                             ty: ty.clone()
                        };
                        let field_addr_reg = self.generate_node_address(field_node_for_addr);

                        // 2. Evaluate the RHS expression (val)
                        let value_reg = self.generate_node(*expr.clone());

                        // 3. Store value_reg into memory at field_addr_reg.
                        // The type for store_memory is the field's type `ty`.
                        self.store_memory(value_reg, field_addr_reg, ty.clone());

                        self.free_register(field_addr_reg);
                        self.assignment_depth -= 1;
                        value_reg // Assignment evaluates to the stored value
                    }
                    _ => panic!("Unexpected LHS token for assignment {:?}", left),
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
                    let reg = self.generate_node(statement);
                    self.free_register(reg);
                }
                0
            }
            Node::WhileStmt { condition, body } => self.while_stmt(condition, body),
            Node::FnDecl {
                identifier,
                body,
                stack_size,
                params,
                ..
            } => self.function(
                identifier,
                params.iter().map(|param| param.borrow().clone()).collect(),
                stack_size,
                body,
            ),
            Node::FnCall { identifier, args, .. } => self.function_call(identifier.clone(), args),
            Node::ReturnStmt { expr, fn_name } => self.return_stmt(expr, fn_name.borrow().clone()),
            Node::PostIncStmt { left } => self.post_inc_stmt(left),
            Node::PostDecStmt { left } => self.post_dec_stmt(left),
            Node::PreIncStmt { right } => self.pre_inc_stmt(right),
            Node::PreDecStmt { right } => self.pre_dec_stmt(right),
            Node::ToBool { expr } => self.expr_to_bool(expr),
            Node::StructDecl { .. } | Node::UnionDecl { .. } | Node::EnumDecl { .. } => {
                // These are type declarations, no direct code generation needed at declaration site.
                0
            }
            // Placeholder for StructFieldAccess
            Node::StructFieldAccess { object, field, ty } => {
                // This handles R-value access (loading the field's value).
                // 1. Get the base address of the struct/union instance.
                let base_addr_reg = self.generate_node_address(*object.clone());

                // 2. Calculate the field's offset from the base address.
                let object_node_ty = (*object).ty().unwrap();
                let field_offset = self.calculate_field_offset(object_node_ty.clone(), &field.lexeme.as_ref().unwrap());

                // 3. Add the offset to the base address. base_addr_reg now holds the field's address.
                self.assembly.text.push_str(&format!(
                    "\taddq\t${}, {}\n",
                    field_offset, REGISTER_NAMES[base_addr_reg]
                ));

                // 4. Load the value from the field's address into a register.
                // The type 'ty' is the type of the field itself.
                self.dereference_memory(base_addr_reg, ty.clone()) // Returns register with the value
            }
        }
    }

    // Generates code that results in a register holding the memory address of the node.
    fn generate_node_address(&mut self, node: Node) -> usize {
        match node {
            Node::LiteralExpr { value, ty: _ } => match value {
                LiteralValue::Identifier(s_rc) => {
                    let symbol_ref = s_rc.borrow();
                    match symbol_ref.class {
                        StorageClass::Local | StorageClass::Param => {
                            let r = self.allocate_register();
                            self.assembly.text.push_str(&format!(
                                "\tleaq\t{}(%rbp), {}\n",
                                symbol_ref.offset.unwrap(), REGISTER_NAMES[r]
                            ));
                            r
                        }
                        StorageClass::Global => {
                            let r = self.allocate_register();
                            self.assembly.text.push_str(&format!(
                                "\tleaq\t{}(%rip), {}\n",
                                symbol_ref.identifier.lexeme.as_ref().unwrap(), REGISTER_NAMES[r]
                            ));
                            r
                        }
                    }
                }
                _ => panic!("Cannot get address of non-identifier literal: {:?}", value),
            },
            Node::UnaryExpr { operator, right, ty: _ } if operator.token_type == TokenType::Mul => {
                // The "address" of *ptr is the value of ptr.
                self.generate_node(*right) // generate_node evaluates the expression, giving value of ptr
            }
            Node::StructFieldAccess { object, field, ty: _field_ty } => {
                let object_base_address_reg = self.generate_node_address(*object.clone());
                let object_ty = (*object).ty().unwrap();

                let field_lexeme = field.lexeme.as_ref().unwrap_or_else(|| panic!("Field token has no lexeme"));
                let field_offset = self.calculate_field_offset(object_ty, field_lexeme);

                self.assembly.text.push_str(&format!(
                    "\taddq\t${}, {}\n",
                    field_offset, REGISTER_NAMES[object_base_address_reg]
                ));
                object_base_address_reg
            }
            _ => panic!("Cannot take address of node type via generate_node_address: {:?}", node),
        }
    }

    fn calculate_field_offset(&self, current_type: Type, field_name: &str) -> usize {
        match current_type {
            Type::Struct { name, fields, .. } => {
                let mut current_offset = 0;
                for (f_name, f_type) in fields {
                    if f_name == field_name {
                        return current_offset;
                    }
                    current_offset += f_type.size();
                }
                panic!("Field '{}' not found in struct '{}'", field_name, name);
            }
            Type::Union { name, fields, .. } => {
                if fields.iter().any(|(fname, _)| fname == field_name) {
                    return 0;
                }
                panic!("Field '{}' not found in union '{}'", field_name, name);
            }
            Type::Pointer { ty, .. } => {
                self.calculate_field_offset(*ty, field_name)
            }
            _ => panic!("Cannot calculate field offset for non-struct/union/pointer type {:?}", current_type),
        }
    }

    fn preamble(&mut self) {
        self.free_all_registers();
        self.assembly.data.push_str("\t.data\n");
        self.assembly.data.push_str("buffer: .space 21\n");

        self.assembly.text.push_str("\t.text\n");

        //         self.assembly.text.push_str(
        //             "
        // .global printint
        // printint:
        //     # Prologue
        //     push %rbp
        //     mov %rsp, %rbp

        //     # Check if input number is 0
        //     cmp $0, %rdi
        //     je print_zero

        //     # Prepare for conversion
        //     mov %rdi, %rax        # Copy number to rax
        //     mov $buffer+20, %rsi  # Load address of the second last digit space
        //     movb $10, (%rsi)     # Store newline character after the number

        //     # Convert number to string
        // convert_loop:
        //     xor %rdx, %rdx        # Clear rdx for div
        //     mov $10, %rbx         # Set divisor to 10
        //     div %rbx              # Divide rax by 10, result in rax, remainder in rdx
        //     add $48, %rdx         # Convert remainder to ASCII
        //     dec %rsi
        //     mov %dl, (%rsi)       # Store ASCII character
        //     test %rax, %rax       # Check if number is zero
        //     jnz convert_loop      # If not, continue loop

        //     # Set up for write syscall
        //     movq $buffer+21, %rdi
        //     subq %rsi, %rdi
        //     movq %rdi, %rdx
        //     mov $1, %rax          # syscall number for sys_write
        //     mov $1, %rdi          # file descriptor 1 (stdout)
        //     syscall

        //     # Epilogue
        //     mov $0, %rax
        //     leave
        //     ret

        // print_zero:
        //     # Handle printing 0
        //     mov $1, %rax          # syscall number for sys_write
        //     mov $1, %rdi          # file descriptor 1 (stdout)
        //     lea buffer+19, %rsi   # point to '0'
        //     movb $'0', (%rsi)     # place '0' in buffer
        //     movb $10, 1(%rsi)     # place newline after '0'
        //     mov $2, %rdx          # length is 2 (including newline)
        //     syscall

        //     mov $0, %rax
        //     leave
        //     ret

        // .global printchar
        // printchar:
        //     # Prologue
        //     push %rbp
        //     mov %rsp, %rbp

        //     # Move character to buffer
        //     lea buffer, %rsi
        //     mov %rdi, (%rsi)

        //     # Set up for write syscall
        //     mov $1, %rax          # syscall number for sys_write
        //     mov $1, %rdi          # file descriptor 1 (stdout)
        //     mov $1, %rdx          # length is 1
        //     syscall

        //     # Epilogue
        //     mov $0, %rax
        //     leave
        //     ret
        // ",
        //         );

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

        self.assembly.text.push_str(".LC1:\n");
        self.assembly.text.push_str("\t.string\t\"%c\"\n");
        self.assembly.text.push_str("printchar:\n");
        self.assembly.text.push_str("\tpushq\t%rbp\n");
        self.assembly.text.push_str("\tmovq\t%rsp, %rbp\n");
        self.assembly.text.push_str("\tsubq\t$16, %rsp\n");
        self.assembly.text.push_str("\tmovl\t%edi, -4(%rbp)\n");
        self.assembly.text.push_str("\tmovl\t-4(%rbp), %eax\n");
        self.assembly.text.push_str("\tmovl\t%eax, %esi\n");
        self.assembly.text.push_str("\tleaq\t.LC1(%rip), %rdi\n");
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

    fn load_global(&mut self, symbol: Symbol, ty: Type) -> usize {
        let r = self.allocate_register();
        let identifier = symbol.identifier.lexeme.unwrap();
        if ty == Type::U8 || ty == Type::Char {
            self.assembly.text.push_str(&format!(
                "\tmovzbq\t{}, {}\n",
                identifier, REGISTER_NAMES[r]
            ));
        } else if ty == Type::U16 {
            self.assembly
                .text
                .push_str(&format!("\tmovzx\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if ty == Type::U32 {
            self.assembly.text.push_str(&format!(
                "\tmov\t{}, {}\n",
                identifier, DWORD_REGISTER_NAMES[r]
            ));
        } else if ty == Type::U64 || matches!(ty, Type::Pointer { .. }) {
            self.assembly
                .text
                .push_str(&format!("\tmovq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if ty == Type::I8 {
            self.assembly.text.push_str(&format!(
                "\tmovsbq\t{}, {}\n",
                identifier, REGISTER_NAMES[r]
            ));
        } else if ty == Type::I16 {
            self.assembly
                .text
                .push_str(&format!("\tmovsx\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if ty == Type::I32 {
            self.assembly.text.push_str(&format!(
                "\tmov\t{}, {}\n",
                identifier, DWORD_REGISTER_NAMES[r]
            ));
        } else if ty == Type::I64 {
            self.assembly
                .text
                .push_str(&format!("\tmovq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else if let Type::Array { .. } = ty {
            self.assembly
                .text
                .push_str(&format!("\tleaq\t{}, {}\n", identifier, REGISTER_NAMES[r]));
        } else {
            panic!("Unexpected type {:?}", ty);
        }

        r
    }

    fn store(&mut self, register: usize, symbol: Symbol, ty: Type) {
        let ty = match ty {
            Type::Array { ty, .. } => ty.pointer_to(),
            _ => ty,
        };
        match symbol.class {
            StorageClass::Local | StorageClass::Param => self.store_local(register, symbol, ty),
            StorageClass::Global => self.store_global(register, symbol, ty),
        }
    }

    fn store_local(&mut self, register: usize, symbol: Symbol, ty: Type) {
        let offset = symbol.offset.unwrap();

        if ty == Type::U8 || ty == Type::Char {
            self.assembly.text.push_str(&format!(
                "\tmovb\t{}, {}(%rbp)\n",
                BYTE_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::U16 {
            self.assembly.text.push_str(&format!(
                "\tmovw\t{}, {}(%rbp)\n",
                WORD_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::U32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}, {}(%rbp)\n",
                DWORD_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::U64 || matches!(ty, Type::Pointer { .. }) {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}, {}(%rbp)\n",
                REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::I8 {
            self.assembly.text.push_str(&format!(
                "\tmovb\t{}, {}(%rbp)\n",
                BYTE_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::I16 {
            self.assembly.text.push_str(&format!(
                "\tmovw\t{}, {}(%rbp)\n",
                WORD_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::I32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}, {}(%rbp)\n",
                DWORD_REGISTER_NAMES[register], offset
            ));
        } else if ty == Type::I64 || matches!(ty, Type::Pointer { .. }) {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}, {}(%rbp)\n",
                REGISTER_NAMES[register], offset
            ));
        } else {
            panic!("Unexpected type {:?}", ty);
        }
    }

    fn store_global(&mut self, register: usize, symbol: Symbol, ty: Type) {
        let identifier = symbol.identifier.lexeme.unwrap();
        if ty == Type::U8 || ty == Type::Char {
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
        } else if ty == Type::U64 || matches!(ty, Type::Pointer { .. }) {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}, {}\n",
                REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::I8 {
            self.assembly.text.push_str(&format!(
                "\tmovb\t{}, {}\n",
                BYTE_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::I16 {
            self.assembly.text.push_str(&format!(
                "\tmovw\t{}, {}\n",
                WORD_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::I32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}, {}\n",
                DWORD_REGISTER_NAMES[register], identifier
            ));
        } else if ty == Type::I64 || matches!(ty, Type::Pointer { .. }) {
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
            1 => "\t.byte\t0\n".to_string(),
            2 => "\t.short\t0\n".to_string(),
            4 => "\t.long\t0\n".to_string(),
            8 => "\t.quad\t0\n".to_string(),
            _ => format!("\t.space\t{}\n", type_size), // Use .space for other sizes
        };

        for _ in 0..count {
            self.assembly.data.push_str(&size_str);
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
            Node::ToBool { expr } => {
                let left_reg = self.generate_node(*expr);
                let right_reg = self.allocate_register();
                self.assembly.text.push_str(&format!(
                    "\ttest\t{}, {}\n",
                    REGISTER_NAMES[right_reg], REGISTER_NAMES[right_reg]
                ));
                (left_reg, right_reg, TokenType::NotEqual)
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
            Node::ToBool { expr } => {
                let left_reg = self.generate_node(*expr);
                let right_reg = self.allocate_register();
                self.assembly
                    .text
                    .push_str(&format!("\tmovq\t$0, {}\n", REGISTER_NAMES[right_reg]));

                (left_reg, right_reg, TokenType::NotEqual)
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

    fn function(
        &mut self,
        identifier: Token,
        params: Vec<Symbol>,
        stack_size: usize,
        body: Box<Node>,
    ) -> usize {
        let fn_name = identifier.lexeme.unwrap();
        self.function_preamble(fn_name.clone(), params, stack_size);
        self.generate_node(*body);
        self.function_postamble(fn_name.clone());
        0
    }

    fn function_preamble(&mut self, name: String, mut params: Vec<Symbol>, stack_size: usize) {
        let mut param_reg = FIRST_PARAM_REG;

        self.assembly
            .text
            .push_str(&format!("\t.global {}\n", name));
        self.assembly
            .text
            .push_str(&format!("\t.type\t{}, @function\n", name));
        self.assembly.text.push_str(&format!("{}:\n", name));
        self.assembly.text.push_str("\tpushq\t%rbp\n");
        self.assembly.text.push_str("\tmovq\t%rsp, %rbp\n");

        // Copy any in-register parameters to the stack
        // Stop after six parameter registers
        for (i, param) in params.iter_mut().enumerate() {
            if i >= 6 {
                break;
            }

            self.store_local(param_reg as usize, param.clone(), param.clone().ty.unwrap());

            param_reg -= 1;
        }

        // Align the stack pointer to be a multiple of 16
        // less than its previous value
        self.stack_offset = (self.local_offset + stack_size as isize + 15) & !15;

        self.assembly
            .text
            .push_str(&format!("\taddq\t${}, %rsp\n", -self.stack_offset));
    }

    fn function_postamble(&mut self, fn_name: String) {
        self.assembly
            .text
            .push_str(format!("{}_end:\n", fn_name).as_str());

        self.assembly
            .text
            .push_str(&format!("\taddq\t${}, %rsp\n", self.stack_offset));
        self.assembly.text.push_str("\tpopq\t%rbp\n");
        self.assembly.text.push_str("\tret\n");

        self.local_offset = 0;
        self.stack_offset = 0;
    }

    fn function_call(&mut self, identifier: crate::lexer::Token, args: Vec<Node>) -> usize {
        for (i, arg) in args.iter().rev().enumerate() {
            let register = self.generate_node(arg.clone());
            self.copy_arg(register, args.len() - i);
            self.free_register(register);
        }

        self.assembly
            .text
            .push_str(&format!("\tcall\t{}\n", identifier.lexeme.unwrap()));

        if args.len() > 6 {
            self.assembly
                .text
                .push_str(&format!("\taddq\t${}, %rsp\n", 8 * (args.len() - 6)));
        }

        let out_register = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t%rax, {}\n", REGISTER_NAMES[out_register]));
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
            Type::U64 | Type::Pointer { .. } => {
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

    // address_of is effectively replaced by generate_node_address for most cases.
    // If direct symbol address is needed internally, new specific functions can be made.
    // Keeping this commented out for now to ensure it's not called unexpectedly.
    // fn address_of(&mut self, symbol: Symbol) -> usize {
    //     let r = self.allocate_register();
    //     match symbol.class {
    //         StorageClass::Global => {
    //             self.assembly.text.push_str(&format!(
    //                 "\tleaq\t{}(%rip), {}\n",
    //                 symbol.identifier.lexeme.unwrap(),
    //                 REGISTER_NAMES[r]
    //             ));
    //         }
    //         StorageClass::Local | StorageClass::Param => {
    //             let offset = symbol.offset.unwrap();
    //             self.assembly.text.push_str(&format!(
    //                 "\tleaq\t{}(%rbp), {}\n",
    //                 offset, REGISTER_NAMES[r]
    //             ));
    //         }
    //     }
    //     r
    // }

    // Renamed: dereference_pointer_value. Takes a register holding a POINTER.
    // Dereferences it and loads the value from pointed-to memory into the SAME register.
    // 'ty_ptr' is the type of the pointer itself (e.g., Pointer to U8).
    fn dereference_pointer_value(&mut self, register_with_pointer: usize, ty_ptr: Type) -> usize {
        let pointed_to_type = ty_ptr.value_at(); // Get the type the pointer points to.
        let size = pointed_to_type.size();

        match size {
            1 => self.assembly.text.push_str(&format!(
                "\tmovzbq\t({}), {}\n",
                REGISTER_NAMES[register_with_pointer], REGISTER_NAMES[register_with_pointer]
            )),
            2 => self.assembly.text.push_str(&format!(
                "\tmovzwq\t({}), {}\n", // movzwq for word to quad
                REGISTER_NAMES[register_with_pointer], REGISTER_NAMES[register_with_pointer]
            )),
            4 => {
                // For I32, sign-extend. For U32, zero-extend.
                if pointed_to_type.is_int() && format!("{:?}", pointed_to_type).starts_with('I') {
                     self.assembly.text.push_str(&format!(
                        "\tmovslq\t({}), {}\n", // sign-extend long to quad
                        REGISTER_NAMES[register_with_pointer], REGISTER_NAMES[register_with_pointer]
                    ));
                } else { // U32 or Enum (treated as unsigned for load)
                     self.assembly.text.push_str(&format!(
                        "\tmovl\t({}), {}\n", // movl from memory to 32-bit part of reg zero-extends to 64-bit
                        REGISTER_NAMES[register_with_pointer], DWORD_REGISTER_NAMES[register_with_pointer]
                    ));
                }
            }
            8 => self.assembly.text.push_str(&format!(
                "\tmovq\t({}), {}\n",
                REGISTER_NAMES[register_with_pointer], REGISTER_NAMES[register_with_pointer]
            )),
            _ => panic!("Cannot dereference pointer to type {:?} with size {} from memory into register", pointed_to_type, size)
        }
        register_with_pointer
    }

    // New: dereference_memory. Loads data of 'ty_data' from memory address (in address_reg) into address_reg.
    fn dereference_memory(&mut self, address_reg: usize, ty_data: Type) -> usize {
        let size = ty_data.size();
        match size {
            1 => self.assembly.text.push_str(&format!(
                "\tmovzbq\t({}), {}\n",
                REGISTER_NAMES[address_reg], REGISTER_NAMES[address_reg]
            )),
            2 => self.assembly.text.push_str(&format!(
                "\tmovzwq\t({}), {}\n",
                REGISTER_NAMES[address_reg], REGISTER_NAMES[address_reg]
            )),
            4 => {
                if ty_data.is_int() && format!("{:?}", ty_data).starts_with('I') {
                     self.assembly.text.push_str(&format!(
                        "\tmovslq\t({}), {}\n",
                        REGISTER_NAMES[address_reg], REGISTER_NAMES[address_reg]
                    ));
                } else {
                     self.assembly.text.push_str(&format!(
                        "\tmovl\t({}), {}\n",
                         REGISTER_NAMES[address_reg], DWORD_REGISTER_NAMES[address_reg]
                    ));
                }
            }
            8 => self.assembly.text.push_str(&format!(
                "\tmovq\t({}), {}\n",
                REGISTER_NAMES[address_reg], REGISTER_NAMES[address_reg]
            )),
            _ => panic!("Cannot load type {:?} with size {} from memory address into register", ty_data, size)
        }
        address_reg
    }

    fn scale(&mut self, register: usize, value: u8) -> usize {
        self.assembly.text.push_str(&format!(
            "\tsalq\t${}, {}\n",
            value, REGISTER_NAMES[register]
        ));
        register
    }

    // Renamed: store_memory. Stores value from value_register into memory at address_register.
    // ty_at_address is the type of the data at the memory location.
    fn store_memory(&mut self, value_register: usize, address_register: usize, ty_at_address: Type) {
        match ty_at_address.size() {
            1 => self.assembly.text.push_str(&format!(
                "\tmovb\t{}, ({})\n",
                BYTE_REGISTER_NAMES[value_register], REGISTER_NAMES[address_register]
            )),
            2 => self.assembly.text.push_str(&format!(
                "\tmovw\t{}, ({})\n",
                WORD_REGISTER_NAMES[value_register], REGISTER_NAMES[address_register]
            )),
            4 => self.assembly.text.push_str(&format!(
                "\tmovl\t{}, ({})\n",
                DWORD_REGISTER_NAMES[value_register], REGISTER_NAMES[address_register]
            )),
            8 => self.assembly.text.push_str(&format!(
                "\tmovq\t{}, ({})\n",
                REGISTER_NAMES[value_register], REGISTER_NAMES[address_register]
            )),
            _ => panic!("Unexpected type size {} for store_memory of type {:?}", ty_at_address.size(), ty_at_address),
        };
    }

    fn define_string(&mut self, label: String, s: String) {
        self.assembly.data.push_str(&format!("{}:\n", label));
        for c in s.as_bytes() {
            self.assembly.data.push_str(&format!("\t.byte\t{}\n", c));
        }
        self.assembly.data.push_str("\t.byte\t0\n");
    }

    fn load_string(&mut self, label: String) -> usize {
        let r = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tleaq\t{}, {}\n", label, REGISTER_NAMES[r]));
        r
    }

    fn post_inc_stmt(&mut self, left: Box<Node>) -> usize {
        // should increment the value and return the old value
        let left = match *left.clone() {
            Node::LiteralExpr {
                value: LiteralValue::Identifier(s),
                ..
            } => s,
            _ => panic!("Unexpected token {:?}", left),
        };

        match left.clone().borrow().class {
            StorageClass::Global => {
                let left = {
                    let borrowed = left.borrow();
                    borrowed.identifier.lexeme.clone().unwrap()
                };
                let r = self.allocate_register();
                let r2 = self.allocate_register();
                self.assembly
                    .text
                    .push_str(&format!("\tmovq\t{}, {}\n", left, REGISTER_NAMES[r]));
                self.assembly.text.push_str(&format!(
                    "\tmovq\t{}, {}\n",
                    REGISTER_NAMES[r], REGISTER_NAMES[r2]
                ));
                self.assembly
                    .text
                    .push_str(&format!("\taddq\t$1, {}\n", REGISTER_NAMES[r]));
                self.assembly
                    .text
                    .push_str(&format!("\tmovq\t{}, {}\n", REGISTER_NAMES[r], left));
                self.free_register(r);
                r2
            }
            StorageClass::Local | StorageClass::Param => {
                let offset = match left.borrow().offset {
                    Some(s) => s,
                    None => panic!("Symbol not found"),
                };

                let r = self.allocate_register();
                let r2 = self.allocate_register();
                self.assembly.text.push_str(&format!(
                    "\tmovq\t{}(%rbp), {}\n",
                    offset, REGISTER_NAMES[r]
                ));
                self.assembly.text.push_str(&format!(
                    "\tmovq\t{}, {}\n",
                    REGISTER_NAMES[r], REGISTER_NAMES[r2]
                ));
                self.assembly
                    .text
                    .push_str(&format!("\taddq\t$1, {}\n", REGISTER_NAMES[r]));
                self.assembly.text.push_str(&format!(
                    "\tmovq\t{}, {}(%rbp)\n",
                    REGISTER_NAMES[r], offset
                ));
                self.free_register(r);
                r2
            }
        }
    }

    fn post_dec_stmt(&mut self, left: Box<Node>) -> usize {
        // should decrement the value and return the old value
        let left = match *left {
            Node::LiteralExpr {
                value: LiteralValue::Identifier(i),
                ..
            } => i,
            _ => panic!("Unexpected token {:?}", left),
        };

        let binding = left.borrow();
        let left = binding.identifier.lexeme.as_ref().unwrap();
        let r = self.allocate_register();
        let r2 = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", left, REGISTER_NAMES[r]));
        self.assembly.text.push_str(&format!(
            "\tmovq\t{}, {}\n",
            REGISTER_NAMES[r], REGISTER_NAMES[r2]
        ));
        self.assembly
            .text
            .push_str(&format!("\tsubq\t$1, {}\n", REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", REGISTER_NAMES[r], left));
        self.free_register(r);
        r2
    }

    fn pre_inc_stmt(&mut self, right: Box<Node>) -> usize {
        let right = match *right.clone() {
            Node::LiteralExpr {
                value: LiteralValue::Identifier(i),
                ..
            } => i,
            _ => panic!("Unexpected token {:?}", right),
        };

        let binding = right.borrow();
        let right = binding.identifier.lexeme.as_ref().unwrap();
        let r = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", right, REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\taddq\t$1, {}\n", REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", REGISTER_NAMES[r], right));
        r
    }

    fn pre_dec_stmt(&mut self, right: Box<Node>) -> usize {
        let right = match *right.clone() {
            Node::LiteralExpr {
                value: LiteralValue::Identifier(i),
                ..
            } => i,
            _ => panic!("Unexpected token {:?}", right),
        };

        let binding = right.borrow();
        let right = binding.identifier.lexeme.as_ref().unwrap();
        let r = self.allocate_register();
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", right, REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\tsubq\t$1, {}\n", REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\tmovq\t{}, {}\n", REGISTER_NAMES[r], right));
        r
    }

    fn expr_to_bool(&mut self, expr: Box<Node>) -> usize {
        let r = self.generate_node(*expr);
        self.assembly
            .text
            .push_str(&format!("\tcmpq\t$0, {}\n", REGISTER_NAMES[r]));
        self.assembly
            .text
            .push_str(&format!("\tsetne\t{}\n", BYTE_REGISTER_NAMES[r]));
        self.assembly.text.push_str(&format!(
            "\tmovzbq\t{}, {}\n",
            BYTE_REGISTER_NAMES[r], REGISTER_NAMES[r]
        ));
        r
    }

    fn negate(&mut self, right_node: usize) -> usize {
        self.assembly
            .text
            .push_str(&format!("\tnegq\t{}\n", REGISTER_NAMES[right_node]));
        right_node
    }

    fn invert(&mut self, right_node: usize) -> usize {
        self.assembly
            .text
            .push_str(&format!("\tnotq\t{}\n", REGISTER_NAMES[right_node]));
        right_node
    }

    fn logical_not(&mut self, right_node: usize) -> usize {
        self.assembly.text.push_str(&format!(
            "\ttestq\t{}, {}\n",
            REGISTER_NAMES[right_node], REGISTER_NAMES[right_node]
        ));
        self.assembly
            .text
            .push_str(&format!("\tsete\t{}\n", BYTE_REGISTER_NAMES[right_node]));
        self.assembly.text.push_str(&format!(
            "\tmovzbq\t{}, {}\n",
            BYTE_REGISTER_NAMES[right_node], REGISTER_NAMES[right_node]
        ));
        right_node
    }

    fn load_local(&mut self, symbol: Symbol, ty: Type) -> usize {
        let offset = match symbol.offset {
            Some(s) => s,
            None => panic!("Symbol not found"),
        };

        let r = self.allocate_register();
        if ty == Type::U8 || ty == Type::Char {
            self.assembly.text.push_str(&format!(
                "\tmovzb\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if ty == Type::U16 {
            self.assembly.text.push_str(&format!(
                "\tmovzw\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if ty == Type::U32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}(%rbp), {}\n",
                offset, DWORD_REGISTER_NAMES[r]
            ));
        } else if ty == Type::U64 || matches!(ty, Type::Pointer { .. }) {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if ty == Type::I8 {
            self.assembly.text.push_str(&format!(
                "\tmovsbq\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if ty == Type::I16 {
            self.assembly.text.push_str(&format!(
                "\tmovsx\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if ty == Type::I32 {
            self.assembly.text.push_str(&format!(
                "\tmovl\t{}(%rbp), {}\n",
                offset, DWORD_REGISTER_NAMES[r]
            ));
        } else if ty == Type::I64 {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if matches!(ty, Type::Array { .. } | Type::Struct { .. } | Type::Union { .. }) {
            // For arrays, structs, unions, load their base address
            self.assembly.text.push_str(&format!(
                "\tleaq\t{}(%rbp), {}\n",
                offset, REGISTER_NAMES[r]
            ));
        } else if matches!(ty, Type::Enum{..}) {
             // Enums are loaded like their base integer type.
             // Assuming I32 for enums as per Type definition for now.
             self.assembly.text.push_str(&format!(
                "\tmovl\t{}(%rbp), {}\n",
                offset, DWORD_REGISTER_NAMES[r]
            ));
        }
        else {
            panic!("Unexpected type {:?} for load_local", ty);
        }

        r
    }

    fn logical_and(&self, _left: usize, _right: usize, _ty: Type) -> usize {
        todo!()
    }

    fn logical_or(&self, _left: usize, _right: usize, _ty: Type) -> usize {
        todo!()
    }

    fn bitwise_and(&mut self, left: usize, right: usize, _ty: Type) -> usize {
        self.assembly.text.push_str(&format!(
            "\tandq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn bitwise_or(&mut self, left: usize, right: usize, _ty: Type) -> usize {
        self.assembly.text.push_str(&format!(
            "\torq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn bitwise_xor(&mut self, left: usize, right: usize, _ty: Type) -> usize {
        self.assembly.text.push_str(&format!(
            "\txorq\t{}, {}\n",
            REGISTER_NAMES[left], REGISTER_NAMES[right]
        ));
        self.free_register(left);
        right
    }

    fn shift_left(&mut self, left: usize, right: usize, ty: Type) -> usize {
        let (c_register, mov, r) = match ty.size() {
            1 => ("%cl", "mov", BYTE_REGISTER_NAMES[right]),
            2 => ("%cx", "movw", WORD_REGISTER_NAMES[right]),
            4 => ("%ecx", "movl", DWORD_REGISTER_NAMES[right]),
            8 => ("%rcx", "movq", REGISTER_NAMES[right]),
            _ => panic!("Unexpected type {:?}", ty),
        };
        self.assembly
            .text
            .push_str(&format!("\t{}\t {}, {}\n", mov, r, c_register));
        self.assembly.text.push_str(&format!(
            "\tsalq\t {}, {}\n",
            c_register, REGISTER_NAMES[left]
        ));
        self.free_register(right);
        left
    }

    fn shift_right(&mut self, left: usize, right: usize, ty: Type) -> usize {
        let (c_register, mov, r) = match ty.size() {
            1 => ("%cl", "mov", BYTE_REGISTER_NAMES[right]),
            2 => ("%cx", "movw", WORD_REGISTER_NAMES[right]),
            4 => ("%ecx", "movl", DWORD_REGISTER_NAMES[right]),
            8 => ("%rcx", "movq", REGISTER_NAMES[right]),
            _ => panic!("Unexpected type {:?}", ty),
        };
        self.assembly
            .text
            .push_str(&format!("\t{}\t {}, {}\n", mov, r, c_register));
        let shift = match ty {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => "sarq",
            _ => "shrq",
        };
        self.assembly.text.push_str(&format!(
            "\t{}\t {}, {}\n",
            shift, c_register, REGISTER_NAMES[left]
        ));
        self.free_register(right);
        left
    }

    fn copy_arg(&mut self, register: usize, arg_pos: usize) {
        /*
        FIRST_PARAM_REG = 9
        const REGISTER_NAMES: [&str; 10] = ["%r10", "%r11", "%r12", "%r13", "%r9", "%r8", "%rcx", "%rdx", "%rsi", "%rdi"];

         */
        if arg_pos > 6 {
            self.assembly
                .text
                .push_str(&format!("\tpushq\t{}\n", REGISTER_NAMES[register]));
        } else {
            self.assembly.text.push_str(&format!(
                "\tmovq\t{}, {}\n",
                REGISTER_NAMES[register],
                REGISTER_NAMES[(FIRST_PARAM_REG - (arg_pos as isize) + 1) as usize]
            ));
        }
    }
}
