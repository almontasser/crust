use crate::ast::Node;

pub struct CodeGen {
    nodes: Vec<Node>,
    assembly: String,
    registers: [bool; 4],
}

const REGISTER_NAMES: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];

impl CodeGen {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self {
            nodes,
            assembly: String::new(),
            registers: [false; 4],
        }
    }

    pub fn generate(&mut self) -> String {
        self.preamble();

        // for node in self.nodes.clone() {
        //     let register = self.generate_node(node);
        // }
        let reg = self.generate_node(self.nodes[0].clone());
        self.printint(reg);

        self.postamble();

        self.assembly.clone()
    }

    fn generate_node(&mut self, node: Node) -> usize {
        match node {
            Node::LiteralExpr { value } => match value {
                crate::lexer::Literal::Integer(i) => self.load(i as i64),
            },
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                let left = self.generate_node(*left);
                let right = self.generate_node(*right);

                match operator.token_type {
                    crate::lexer::TokenType::Add => self.add(left, right),
                    crate::lexer::TokenType::Sub => self.subtract(left, right),
                    crate::lexer::TokenType::Mul => self.multiply(left, right),
                    crate::lexer::TokenType::Div => self.divide(left, right),
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
            Node::UnaryExpr { operator, right } => {
                let right = self.generate_node(*right);

                match operator.token_type {
                    crate::lexer::TokenType::Sub => {
                        self.load(0);
                        self.subtract(0, right)
                    }
                    _ => panic!("Unexpected token {:?}", operator),
                }
            }
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
}
