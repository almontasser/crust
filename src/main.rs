use std::io::Write;

use ast::Node;
use codegen::CodeGen;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod types;
mod utils;

fn _print_node(node: Node, ident: u8) {
    for _ in 0..ident {
        print!("  ");
    }

    match node {
        Node::BinaryExpr {
            left,
            operator,
            right,
            ..
        } => {
            println!("BinaryExpr");
            _print_node(*left, ident + 1);
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("operator: {:?}", operator);
            _print_node(*right, ident + 1);
        }
        Node::UnaryExpr {
            operator,
            right,
            ty,
        } => {
            println!("UnaryExpr");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("operator: {:?}", operator);
            _print_node(*right, ident + 1);
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("ty: {:?}", ty)
        }
        Node::WidenExpr { right, .. } => {
            println!("WidenExpr");
            _print_node(*right, ident + 1);
        }
        Node::ScaleExpr { right, size, .. } => {
            println!("ScaleExpr");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("size: {}", size);
            _print_node(*right, ident + 1);
        }
        Node::LiteralExpr { value, .. } => {
            println!("LiteralExpr");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("value: {:?}", value);
        }
        Node::VarDecl { symbol, ty, .. } => {
            println!("GlobalVar");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("{}: {:?}", symbol.identifier.lexeme.unwrap(), ty)
        }
        Node::VarDeclMany { symbols, ty, .. } => {
            println!("GlobalVarMany");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            for symbol in symbols {
                println!("{}: {:?}", symbol.identifier.lexeme.unwrap(), ty)
            }
        }
        Node::AssignStmt { left, expr } => {
            println!("AssignStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("left:");
            _print_node(*left, ident + 2);
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("expr:");
            _print_node(*expr, ident + 2);
        }
        Node::CompoundStmt { statements } => {
            println!("CompoundStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("statements:");
            for statement in statements {
                _print_node(statement, ident + 2);
            }
        }
        Node::IfStmt {
            condition,
            then_branch,
            else_branch,
        } => {
            println!("IfStmt");
            _print_node(*condition, ident + 1);
            _print_node(*then_branch, ident + 1);
            if let Some(else_branch) = else_branch {
                _print_node(*else_branch, ident + 1);
            }
        }
        Node::WhileStmt { condition, body } => {
            println!("WhileStmt");
            _print_node(*condition, ident + 1);
            _print_node(*body, ident + 1);
        }
        Node::FnDecl {
            identifier,
            body,
            return_type,
            ..
        } => {
            println!("FnDecl");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!(
                "{}(): {:?}",
                identifier.lexeme.unwrap(),
                return_type.unwrap()
            );

            _print_node(*body, ident + 1);
        }
        Node::FnCall {
            identifier, expr, ..
        } => {
            println!("FnCall");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifier: {:?}", identifier);
            _print_node(*expr, ident + 1);
        }
        Node::ReturnStmt { expr, fn_name } => {
            println!("ReturnStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("fn_name: {:?}", fn_name);
            _print_node(*expr, ident + 1);
        }
        Node::PostIncStmt { left } => {
            println!("PostIncStmt");
            _print_node(*left, ident + 1);
        }
        Node::PostDecStmt { left } => {
            println!("PostDecStmt");
            _print_node(*left, ident + 1);
        }
        Node::PreIncStmt { right } => {
            println!("PreIncStmt");
            _print_node(*right, ident + 1);
        }
        Node::PreDecStmt { right } => {
            println!("PreDecStmt");
            _print_node(*right, ident + 1);
        }
        Node::ToBool { expr } => {
            println!("ToBool");
            _print_node(*expr, ident + 1);
        }
    }
}

const DEBUG: bool = false;
const DEBUG_TEST_FILE: &str = "tests/test24";

fn main() {
    let source = if !DEBUG {
        let args: Vec<String> = std::env::args().collect();
        if args.len() != 2 {
            println!("Usage: crust <source>");
            std::process::exit(1);
        }

        std::fs::read_to_string(&args[1]).expect("Failed to read file")
    } else {
        std::fs::read_to_string(DEBUG_TEST_FILE).expect("Failed to read file")
    };

    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    if DEBUG {
        for node in nodes.clone() {
            _print_node(node, 0);
        }
    }

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    // write assembly to file
    let mut output_file = std::fs::File::create("out.s").expect("Failed to create file");
    output_file
        .write_all(assembly.as_bytes())
        .expect("Failed to write to file");
}
