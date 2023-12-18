use codegen::CodeGen;

mod ast;
mod codegen;
mod lexer;
mod parser;

fn main() {
    let source = String::from(
        "{
            let i; let j;
            i=6; j=12;
            if (i < j) {
              print(i);
            } else {
              print(j);
            }
            let x;
          }",
    );
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    println!("{}", assembly);
}
