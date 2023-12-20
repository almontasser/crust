use std::io::Write;

use codegen::CodeGen;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod types;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: zcompiler <source>");
        std::process::exit(1);
    }

    let source = std::fs::read_to_string(&args[1]).expect("Failed to read file");

    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    // write assembly to file
    let mut output_file = std::fs::File::create("out.s").expect("Failed to create file");
    output_file
        .write_all(assembly.as_bytes())
        .expect("Failed to write to file");
}
