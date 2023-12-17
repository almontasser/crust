use crate::{
    ast::Node,
    lexer::{Literal, TokenType},
};

pub struct CodeGen {
    nodes: Vec<Node>,
    assembly: String,
    registers: [bool; 4],
}

const REGISTER_NAMES: [&str; 4] = ["%r8", "%r9", "%r10", "%r11"];
const BYTE_REGISTER_NAMES: [&str; 4] = ["%r8b", "%r9b", "%r10b", "%r11b"];

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
                    TokenType::Equal => self.equal(left, right),
                    TokenType::NotEqual => self.not_equal(left, right),
                    TokenType::LessThan => self.less_than(left, right),
                    TokenType::LessThanOrEqual => self.less_than_or_equal(left, right),
                    TokenType::GreaterThan => self.greater_than(left, right),
                    TokenType::GreaterThanOrEqual => self.greater_than_or_equal(left, right),
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

    fn compare(&mut self, left: usize, right: usize, instruction: &str) -> usize {
        self.assembly.push_str(&format!(
            "\tcmpq\t{}, {}\n",
            REGISTER_NAMES[right], REGISTER_NAMES[left]
        ));
        self.assembly.push_str(&format!(
            "\t{} {}\n",
            instruction, BYTE_REGISTER_NAMES[right]
        ));
        self.assembly
            .push_str(&format!("\tandq\t$255, {}\n", REGISTER_NAMES[right]));
        self.free_register(left);
        right
    }

    fn equal(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "sete")
    }

    fn not_equal(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "setne")
    }

    fn less_than(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "setl")
    }

    fn less_than_or_equal(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "setle")
    }

    fn greater_than(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "setg")
    }

    fn greater_than_or_equal(&mut self, left: usize, right: usize) -> usize {
        self.compare(left, right, "setge")
    }
}
