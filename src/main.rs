//! Crust Compiler - A compiler for a modified Rust-like language

mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod span;

use codegen::CodeGen;
use errors::ErrorReporter;
use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    
    let (input_file, output_file) = if args.len() >= 2 {
        let input = args[1].clone();
        let output = if args.len() >= 3 {
            args[2].clone()
        } else {
            // Default output name: remove extension and use as binary name
            input.strip_suffix(".cr").unwrap_or(&input).to_string()
        };
        (input, output)
    } else {
        eprintln!("\x1b[1;31merror\x1b[0m: no input file specified");
        eprintln!("\x1b[1mUsage\x1b[0m: crust <input.cr> [output]");
        return ExitCode::from(1);
    };
    
    // Read source file
    let source = match fs::read_to_string(&input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("\x1b[1;31merror\x1b[0m: could not read file `{}`: {}", input_file, e);
            return ExitCode::from(1);
        }
    };
    
    let reporter = ErrorReporter::new(&source, &input_file);
    
    // Lexing
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    
    // Check for lexer errors
    let lexer_diagnostics = lexer.take_diagnostics();
    if !lexer_diagnostics.is_empty() {
        reporter.report(&lexer_diagnostics);
        return ExitCode::from(1);
    }
    
    // Parsing
    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(program) => program,
        Err(diagnostics) => {
            reporter.report(&diagnostics);
            return ExitCode::from(1);
        }
    };
    
    // Code generation
    let mut codegen = CodeGen::new();
    let assembly = match codegen.generate(&program) {
        Ok(asm) => asm,
        Err(diagnostics) => {
            reporter.report(&diagnostics);
            return ExitCode::from(1);
        }
    };
    
    // Write assembly to file
    let asm_file = format!("{}.s", output_file);
    if let Err(e) = fs::write(&asm_file, &assembly) {
        eprintln!("\x1b[1;31merror\x1b[0m: could not write assembly file: {}", e);
        return ExitCode::from(1);
    }
    
    // Assemble using system assembler
    let obj_file = format!("{}.o", output_file);
    let as_status = match Command::new("as")
        .args(["-o", &obj_file, &asm_file])
        .status() 
    {
        Ok(status) => status,
        Err(e) => {
            eprintln!("\x1b[1;31merror\x1b[0m: failed to run assembler: {}", e);
            let _ = fs::remove_file(&asm_file);
            return ExitCode::from(1);
        }
    };
    
    if !as_status.success() {
        eprintln!("\x1b[1;31merror\x1b[0m: assembly failed");
        let _ = fs::remove_file(&asm_file);
        return ExitCode::from(1);
    }
    
    // Link using system linker
    let ld_status = match Command::new("ld")
        .args([
            "-o", &output_file,
            &obj_file,
            "-lSystem",
            "-syslibroot", "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk",
            "-e", "_main",
            "-arch", "arm64",
        ])
        .status()
    {
        Ok(status) => status,
        Err(e) => {
            eprintln!("\x1b[1;31merror\x1b[0m: failed to run linker: {}", e);
            let _ = fs::remove_file(&asm_file);
            let _ = fs::remove_file(&obj_file);
            return ExitCode::from(1);
        }
    };
    
    if !ld_status.success() {
        eprintln!("\x1b[1;31merror\x1b[0m: linking failed");
        let _ = fs::remove_file(&asm_file);
        let _ = fs::remove_file(&obj_file);
        return ExitCode::from(1);
    }
    
    // Clean up intermediate files
    let _ = fs::remove_file(&asm_file);
    let _ = fs::remove_file(&obj_file);
    
    eprintln!("\x1b[1;32mCompiled\x1b[0m {} -> {}", input_file, output_file);
    
    ExitCode::SUCCESS
}
