use ast::Node;
use codegen::CodeGen;
use lexer::Literal;

mod ast;
mod codegen;
mod lexer;
mod parser;

fn interpre(node: Node) -> i64 {
    match node {
        Node::LiteralExpr { value } => match value {
            Literal::Integer(i) => i as i64,
        },
        Node::BinaryExpr {
            left,
            operator,
            right,
        } => {
            let left = interpre(*left);
            let right = interpre(*right);

            match operator.token_type {
                lexer::TokenType::Add => left + right,
                lexer::TokenType::Sub => left - right,
                lexer::TokenType::Mul => left * right,
                lexer::TokenType::Div => left / right,
                _ => panic!("Unexpected token {:?}", operator),
            }
        }
        Node::UnaryExpr { operator, right } => {
            let right = interpre(*right);

            match operator.token_type {
                lexer::TokenType::Sub => -right,
                _ => panic!("Unexpected token {:?}", operator),
            }
        }
    }
}

fn main() {
    let source = String::from("2 + 3 * 5 - 8 / 3");
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    println!("{}", assembly);
}
