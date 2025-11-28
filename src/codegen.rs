//! Code generation module - Generates x86-64 assembly for macOS

use crate::ast::{BinOp, Expr, Program, Spanned, Stmt, UnaryOp};
use crate::errors::{Diagnostic, DiagnosticCollector, DiagnosticEmitter, ErrorCode, Label};

pub struct CodeGen {
    output: String,
    diagnostics: DiagnosticCollector,
}

impl CodeGen {
    pub fn new() -> Self {
        CodeGen {
            output: String::new(),
            diagnostics: DiagnosticCollector::new(),
        }
    }
    
    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }
    
    fn emit_line(&mut self, s: &str) {
        self.output.push_str("    ");
        self.output.push_str(s);
        self.output.push('\n');
    }
    
    pub fn generate(&mut self, program: &Program) -> Result<String, Vec<Diagnostic>> {
        // macOS requires underscore prefix for symbols
        self.emit(".global _main");
        self.emit(".align 2");
        self.emit("");
        
        // Generate print_int function (prints integer in x0)
        self.generate_print_int();
        
        // Generate all functions
        for func in &program.functions {
            self.emit(&format!("_{}:", func.name));
            
            // Function prologue
            self.emit_line("stp x29, x30, [sp, #-16]!");
            self.emit_line("mov x29, sp");
            
            // Generate function body
            for stmt in &func.body {
                if self.generate_stmt(stmt).is_err() {
                    // Continue generating to collect more errors
                }
            }
            
            // Function epilogue
            self.emit_line("mov x0, #0");
            self.emit_line("ldp x29, x30, [sp], #16");
            self.emit_line("ret");
            self.emit("");
        }
        
        if self.diagnostics.has_errors() {
            Err(self.diagnostics.take_diagnostics())
        } else {
            Ok(self.output.clone())
        }
    }
    
    fn generate_print_int(&mut self) {
        self.emit("_print_int:");
        // Save registers
        self.emit_line("stp x29, x30, [sp, #-16]!");
        self.emit_line("mov x29, sp");
        self.emit_line("sub sp, sp, #32");
        
        // Handle negative numbers
        self.emit_line("mov x9, x0");           // save original number
        self.emit_line("cmp x0, #0");
        self.emit_line("bge 1f");               // if positive, skip
        self.emit_line("neg x9, x0");           // make positive for printing
        self.emit_line("mov x0, #'-'");
        self.emit_line("mov x1, sp");
        self.emit_line("strb w0, [x1]");
        self.emit_line("mov x0, #1");           // stdout
        self.emit_line("mov x2, #1");           // length
        self.emit_line("mov x16, #4");          // write syscall
        self.emit_line("svc #0x80");
        self.emit_line("mov x0, x9");           // restore positive number
        
        self.emit("1:");
        self.emit_line("mov x10, sp");          // buffer pointer
        self.emit_line("add x10, x10, #20");    // end of buffer
        self.emit_line("mov x11, x10");         // save end position
        self.emit_line("mov x12, #10");         // divisor
        
        self.emit("2:");
        self.emit_line("udiv x13, x0, x12");    // x13 = x0 / 10
        self.emit_line("msub x14, x13, x12, x0"); // x14 = x0 % 10
        self.emit_line("add x14, x14, #'0'");   // convert to ASCII
        self.emit_line("sub x10, x10, #1");     // move buffer pointer back
        self.emit_line("strb w14, [x10]");      // store digit
        self.emit_line("mov x0, x13");          // x0 = x0 / 10
        self.emit_line("cbnz x0, 2b");          // if not zero, continue
        
        // Print the number
        self.emit_line("mov x0, #1");           // stdout
        self.emit_line("mov x1, x10");          // buffer start
        self.emit_line("sub x2, x11, x10");     // length
        self.emit_line("mov x16, #4");          // write syscall
        self.emit_line("svc #0x80");
        
        // Print newline
        self.emit_line("mov x0, #'\\n'");
        self.emit_line("strb w0, [sp]");
        self.emit_line("mov x0, #1");
        self.emit_line("mov x1, sp");
        self.emit_line("mov x2, #1");
        self.emit_line("mov x16, #4");
        self.emit_line("svc #0x80");
        
        // Restore and return
        self.emit_line("add sp, sp, #32");
        self.emit_line("ldp x29, x30, [sp], #16");
        self.emit_line("ret");
        self.emit("");
    }
    
    fn generate_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), ()> {
        match &stmt.node {
            Stmt::Expr(expr) => {
                self.generate_expr(expr)?;
            }
            Stmt::Let { name, name_span, value } => {
                // For now, we don't support local variables
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::CodeGenError, "local variables not yet supported")
                        .with_label(Label::primary(*name_span, format!("`{}` declared here", name)))
                        .with_note("this feature will be implemented in a future version")
                );
                return Err(());
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.generate_expr(e)?;
                } else {
                    self.emit_line("mov x0, #0");
                }
                self.emit_line("ldp x29, x30, [sp], #16");
                self.emit_line("ret");
            }
        }
        Ok(())
    }
    
    fn generate_expr(&mut self, expr: &Spanned<Expr>) -> Result<(), ()> {
        match &expr.node {
            Expr::Integer(n) => {
                if *n >= 0 && *n < 65536 {
                    self.emit_line(&format!("mov x0, #{}", n));
                } else if *n < 0 && *n > -65536 {
                    self.emit_line(&format!("mov x0, #{}", -n));
                    self.emit_line("neg x0, x0");
                } else {
                    // For larger numbers, use movz/movk
                    let val = *n as u64;
                    self.emit_line(&format!("movz x0, #{}", val & 0xFFFF));
                    if (val >> 16) & 0xFFFF != 0 {
                        self.emit_line(&format!("movk x0, #{}, lsl #16", (val >> 16) & 0xFFFF));
                    }
                    if (val >> 32) & 0xFFFF != 0 {
                        self.emit_line(&format!("movk x0, #{}, lsl #32", (val >> 32) & 0xFFFF));
                    }
                    if (val >> 48) & 0xFFFF != 0 {
                        self.emit_line(&format!("movk x0, #{}, lsl #48", (val >> 48) & 0xFFFF));
                    }
                }
            }
            Expr::Boolean(b) => {
                self.emit_line(&format!("mov x0, #{}", if *b { 1 } else { 0 }));
            }
            Expr::String(_) => {
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::CodeGenError, "string literals not yet supported in expressions")
                        .with_label(Label::primary(expr.span, "string literal here"))
                        .with_note("this feature will be implemented in a future version")
                );
                return Err(());
            }
            Expr::Identifier(name) => {
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::UndefinedVariable, format!("undefined variable `{}`", name))
                        .with_label(Label::primary(expr.span, "not found in this scope"))
                        .with_help("variables must be declared with `let` before use")
                );
                return Err(());
            }
            Expr::BinaryOp { left, op, right } => {
                // Generate right operand first, push to stack
                self.generate_expr(right)?;
                self.emit_line("str x0, [sp, #-16]!");
                
                // Generate left operand
                self.generate_expr(left)?;
                
                // Pop right operand
                self.emit_line("ldr x1, [sp], #16");
                
                // Perform operation
                match op {
                    BinOp::Add => self.emit_line("add x0, x0, x1"),
                    BinOp::Sub => self.emit_line("sub x0, x0, x1"),
                    BinOp::Mul => self.emit_line("mul x0, x0, x1"),
                    BinOp::Div => self.emit_line("sdiv x0, x0, x1"),
                    BinOp::Mod => {
                        self.emit_line("sdiv x2, x0, x1");
                        self.emit_line("msub x0, x2, x1, x0");
                    }
                    BinOp::Eq => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, eq");
                    }
                    BinOp::Ne => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, ne");
                    }
                    BinOp::Lt => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, lt");
                    }
                    BinOp::Le => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, le");
                    }
                    BinOp::Gt => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, gt");
                    }
                    BinOp::Ge => {
                        self.emit_line("cmp x0, x1");
                        self.emit_line("cset x0, ge");
                    }
                    BinOp::And => {
                        self.emit_line("and x0, x0, x1");
                    }
                    BinOp::Or => {
                        self.emit_line("orr x0, x0, x1");
                    }
                }
            }
            Expr::UnaryOp { op, operand } => {
                self.generate_expr(operand)?;
                match op {
                    UnaryOp::Neg => self.emit_line("neg x0, x0"),
                    UnaryOp::Not => {
                        self.emit_line("cmp x0, #0");
                        self.emit_line("cset x0, eq");
                    }
                }
            }
            Expr::Call { name, name_span, args } => {
                match name.as_str() {
                    "println" => {
                        if args.len() != 1 {
                            self.diagnostics.emit(
                                Diagnostic::error(
                                    ErrorCode::WrongArgumentCount,
                                    format!("function `println` expects 1 argument, found {}", args.len())
                                )
                                .with_label(Label::primary(*name_span, "function called here"))
                                .with_help("println takes exactly one integer argument")
                            );
                            return Err(());
                        }
                        self.generate_expr(&args[0])?;
                        self.emit_line("bl _print_int");
                    }
                    _ => {
                        self.diagnostics.emit(
                            Diagnostic::error(ErrorCode::UndefinedFunction, format!("undefined function `{}`", name))
                                .with_label(Label::primary(*name_span, "function not found"))
                                .with_note("only the built-in function `println` is currently available")
                        );
                        return Err(());
                    }
                }
            }
            Expr::Grouped(inner) => {
                self.generate_expr(inner)?;
            }
        }
        Ok(())
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
    
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.take_diagnostics()
    }
}
