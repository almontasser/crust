use ast::Node;
use codegen::CodeGen;
use lexer::Literal;

mod ast;
mod codegen;
mod lexer;
mod parser;

fn main() {
    let source = String::from(
        "let x;
x= 7 < 9;  print(x);
x= 7 <= 9; print(x);
x= 7 != 9; print(x);
x= 7 == 7; print(x);
x= 7 >= 7; print(x);
x= 7 <= 7; print(x);
x= 9 > 7;  print(x);
x= 9 >= 7; print(x);
x= 1 != 7; print(x);",
    );
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    println!("{}", assembly);
}
