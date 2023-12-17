use ast::Node;
use codegen::CodeGen;
use lexer::Literal;

mod ast;
mod codegen;
mod lexer;
mod parser;

fn main() {
    let source = String::from(
        "print(12 * 3);
    print (
       18 - 2
          * 4); print
    (1 + 2 +
      9 - 5/2 + 3*5
    );",
    );
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    println!("{}", assembly);
}
