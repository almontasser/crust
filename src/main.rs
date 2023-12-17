use ast::Node;
use codegen::CodeGen;
use lexer::Literal;

mod ast;
mod codegen;
mod lexer;
mod parser;

fn main() {
    let source = String::from("2 + 3 * 5 - 8 / 3;");
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    println!("{}", assembly);
}
