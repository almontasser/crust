use std::io::Write;

use ast::Node;
use codegen::CodeGen;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod types;

fn print_node(node: Node, ident: u8) {
    for _ in 0..ident {
        print!("  ");
    }

    match node {
        Node::BinaryExpr {
            left,
            operator,
            right,
            ty,
        } => {
            println!("BinaryExpr");
            print_node(*left, ident + 1);
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("operator: {:?}", operator);
            print_node(*right, ident + 1);
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
            print_node(*right, ident + 1);
        }
        Node::WidenExpr { right, ty } => {
            println!("WidenExpr");
            print_node(*right, ident + 1);
        }
        Node::ScaleExpr { right, size, ty } => {
            println!("ScaleExpr");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("size: {}", size);
            print_node(*right, ident + 1);
        }
        Node::LiteralExpr { value, ty } => {
            println!("LiteralExpr");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("value: {:?}", value);
        }
        Node::GlobalVar { identifier, ty } => {
            println!("GlobalVar");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifier: {:?}", identifier);
        }
        Node::GlobalVarMany { identifiers, ty } => {
            println!("GlobalVarMany");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifiers: {:?}", identifiers);
        }
        Node::AssignStmt { identifier, expr } => {
            println!("AssignStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifier: {:?}", identifier);
            print_node(*expr, ident + 1);
        }
        Node::CompoundStmt { statements } => {
            println!("CompoundStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("statements:");
            for statement in statements {
                print_node(statement, ident + 2);
            }
        }
        Node::IfStmt {
            condition,
            then_branch,
            else_branch,
        } => {
            println!("IfStmt");
            print_node(*condition, ident + 1);
            print_node(*then_branch, ident + 1);
            if let Some(else_branch) = else_branch {
                print_node(*else_branch, ident + 1);
            }
        }
        Node::WhileStmt { condition, body } => {
            println!("WhileStmt");
            print_node(*condition, ident + 1);
            print_node(*body, ident + 1);
        }
        Node::FnDecl {
            identifier,
            body,
            return_type,
        } => {
            println!("FnDecl");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifier: {:?}", identifier);
            if let Some(return_type) = return_type {
                for _ in 0..ident + 1 {
                    print!("  ");
                }
                println!("return_type: {:?}", return_type);
            }
            print_node(*body, ident + 1);
        }
        Node::FnCall {
            identifier,
            expr,
            ty,
        } => {
            println!("FnCall");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("identifier: {:?}", identifier);
            print_node(*expr, ident + 1);
        }
        Node::ReturnStmt { expr, fn_name } => {
            println!("ReturnStmt");
            for _ in 0..ident + 1 {
                print!("  ");
            }
            println!("fn_name: {:?}", fn_name);
            print_node(*expr, ident + 1);
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: zcompiler <source>");
        std::process::exit(1);
    }

    let source = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    // let source = std::fs::read_to_string("tests/test17").expect("Failed to read file");

    // let source = String::from(
    //     "fn main() {
    //         printint(1);
    //       }
    //     ",
    // );

    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan_tokens();
    let mut parser = parser::Parser::new(tokens.clone());
    let nodes = parser.parse();

    // for node in nodes.clone() {
    //     print_node(node, 0);
    // }

    let mut codegen = CodeGen::new(nodes.clone());
    let assembly = codegen.generate();

    // write assembly to file
    let mut output_file = std::fs::File::create("out.s").expect("Failed to create file");
    output_file
        .write_all(assembly.as_bytes())
        .expect("Failed to write to file");
}
