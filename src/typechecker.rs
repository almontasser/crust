//! Type Checker for the Crust compiler
//!
//! This module performs semantic analysis including:
//! - Type inference for expressions
//! - Type checking for operations
//! - Ensuring type consistency in assignments and function calls

use std::collections::HashMap;

use crate::ast::{Expr, Function, Program, SpannedExpr, SpannedStmt, Stmt, TypeAnnotation};
use crate::errors::{Diagnostic, DiagnosticCollector, DiagnosticEmitter, ErrorCode, Label};
use crate::types::Type;

/// Type environment that tracks variable and function types
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Variable types in the current scope
    variables: HashMap<String, Type>,
    /// Function signatures (name -> (param_types, return_type))
    functions: HashMap<String, (Vec<Type>, Type)>,
    /// Stack of scopes for nested blocks
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            variables: HashMap::new(),
            functions: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    /// Enter a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Remove variables that were defined in this scope
            for name in scope.keys() {
                self.variables.remove(name);
            }
        }
    }

    /// Define a variable in the current scope
    pub fn define_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name.clone(), ty);
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, Type::Unit); // Just track that it exists in this scope
        }
    }

    /// Look up a variable's type
    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    /// Define a function signature
    pub fn define_function(&mut self, name: String, param_types: Vec<Type>, return_type: Type) {
        self.functions.insert(name, (param_types, return_type));
    }

    /// Look up a function signature
    pub fn get_function(&self, name: &str) -> Option<&(Vec<Type>, Type)> {
        self.functions.get(name)
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Type checker that validates the AST
pub struct TypeChecker {
    env: TypeEnv,
    diagnostics: DiagnosticCollector,
    /// The expected return type for the current function
    current_return_type: Option<Type>,
    /// Track if we've seen a return statement in the current function
    has_return: bool,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: TypeEnv::new(),
            diagnostics: DiagnosticCollector::new(),
            current_return_type: None,
            has_return: false,
        }
    }

    /// Type check an entire program
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<Diagnostic>> {
        // First pass: register all function signatures
        for func in &program.functions {
            let param_types: Vec<Type> = func
                .params
                .iter()
                .map(|p| self.resolve_type_annotation(&p.type_annotation))
                .collect();

            let return_type = func
                .return_type
                .as_ref()
                .map(|t| self.resolve_type_annotation(t))
                .unwrap_or(Type::Unit);

            self.env
                .define_function(func.name.clone(), param_types, return_type);
        }

        // Validate main function if it exists
        if let Some(main_func) = program.functions.iter().find(|f| f.name == "main") {
            if let Some(sig) = self.env.get_function("main") {
                if !sig.0.is_empty() {
                    self.diagnostics.emit(
                        Diagnostic::error(
                            ErrorCode::InvalidMainSignature,
                            "main function should not take parameters",
                        )
                        .with_label(Label::primary(main_func.name_span, "defined here")),
                    );
                }
                // main can return () or any integer type
                if sig.1 != Type::Unit && !sig.1.is_integer() {
                    self.diagnostics.emit(
                        Diagnostic::error(
                            ErrorCode::InvalidMainSignature,
                            format!("main function should return () or an integer type, found {}", sig.1),
                        )
                        .with_label(Label::primary(main_func.name_span, "defined here")),
                    );
                }
            }
        }

        // Second pass: type check each function body
        for func in &program.functions {
            self.check_function(func);
        }

        if self.diagnostics.has_errors() {
            Err(self.diagnostics.take_diagnostics())
        } else {
            Ok(())
        }
    }

    /// Type check a function
    fn check_function(&mut self, func: &Function) {
        self.env.push_scope();
        self.has_return = false;

        // Get return type
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| self.resolve_type_annotation(t))
            .unwrap_or(Type::Unit);

        self.current_return_type = Some(return_type.clone());

        // Add parameters to scope
        for param in &func.params {
            let param_type = self.resolve_type_annotation(&param.type_annotation);
            self.env.define_variable(param.name.clone(), param_type);
        }

        // Type check body
        for stmt in &func.body {
            self.check_stmt(stmt);
        }

        // Check that non-unit functions have a return
        if return_type != Type::Unit && !self.has_return {
            self.diagnostics.emit(
                Diagnostic::error(
                    ErrorCode::MissingReturnValue,
                    format!(
                        "function '{}' is expected to return {} but has no return statement",
                        func.name, return_type
                    ),
                )
                .with_label(Label::primary(func.name_span, "defined here")),
            );
        }

        self.current_return_type = None;
        self.env.pop_scope();
    }

    /// Type check a statement
    fn check_stmt(&mut self, stmt: &SpannedStmt) {
        match &stmt.node {
            Stmt::Expr(expr) => {
                self.infer_type(expr);
            }
            Stmt::Let {
                name,
                name_span,
                type_annotation,
                value,
            } => {
                let value_type = self.infer_type(value);

                // If there's a type annotation, check that it matches
                if let Some(annotation) = type_annotation {
                    let expected_type = self.resolve_type_annotation(annotation);
                    if value_type != Type::Error
                        && !self.types_compatible(&expected_type, &value_type)
                    {
                        self.diagnostics.emit(
                            Diagnostic::error(
                                ErrorCode::TypeMismatch,
                                format!("expected type {}, found {}", expected_type, value_type),
                            )
                            .with_label(Label::primary(value.span, "has wrong type"))
                            .with_label(Label::secondary(
                                annotation.span,
                                format!("expected {}", expected_type),
                            )),
                        );
                    }
                    self.env.define_variable(name.clone(), expected_type);
                } else {
                    // Infer type from value
                    self.env.define_variable(name.clone(), value_type);
                }

                let _ = name_span; // Suppress unused warning
            }
            Stmt::Return(expr) => {
                self.has_return = true;

                let return_type = expr
                    .as_ref()
                    .map(|e| self.infer_type(e))
                    .unwrap_or(Type::Unit);

                if let Some(expected) = &self.current_return_type {
                    if return_type != Type::Error
                        && !self.types_compatible(expected, &return_type)
                    {
                        let span = expr.as_ref().map(|e| e.span).unwrap_or(stmt.span);
                        self.diagnostics.emit(
                            Diagnostic::error(
                                ErrorCode::ReturnTypeMismatch,
                                format!("expected return type {}, found {}", expected, return_type),
                            )
                            .with_label(Label::primary(span, "wrong return type")),
                        );
                    }
                }
            }
        }
    }

    /// Infer the type of an expression
    fn infer_type(&mut self, expr: &SpannedExpr) -> Type {
        match &expr.node {
            // Integer literals default to i64
            Expr::Integer(_) => Type::I64,
            Expr::Boolean(_) => Type::Bool,
            Expr::String(_) => Type::Str,

            Expr::Identifier(name) => {
                if let Some(ty) = self.env.get_variable(name) {
                    ty.clone()
                } else {
                    self.diagnostics.emit(
                        Diagnostic::error(
                            ErrorCode::UndefinedVariable,
                            format!("undefined variable '{}'", name),
                        )
                        .with_label(Label::primary(expr.span, "not found in this scope")),
                    );
                    Type::Error
                }
            }

            Expr::BinaryOp { left, op, right } => {
                let left_type = self.infer_type(left);
                let right_type = self.infer_type(right);

                // Don't cascade errors
                if left_type == Type::Error || right_type == Type::Error {
                    return Type::Error;
                }

                match Type::binary_op_result(&left_type, op, &right_type) {
                    Some(result_type) => result_type,
                    None => {
                        self.diagnostics.emit(
                            Diagnostic::error(
                                ErrorCode::InvalidOperandType,
                                format!(
                                    "cannot apply operator '{}' to {} and {}",
                                    op.as_str(),
                                    left_type,
                                    right_type
                                ),
                            )
                            .with_label(Label::primary(expr.span, "invalid operation")),
                        );
                        Type::Error
                    }
                }
            }

            Expr::UnaryOp { op, operand } => {
                let operand_type = self.infer_type(operand);

                if operand_type == Type::Error {
                    return Type::Error;
                }

                match Type::unary_op_result(&operand_type, op) {
                    Some(result_type) => result_type,
                    None => {
                        self.diagnostics.emit(
                            Diagnostic::error(
                                ErrorCode::InvalidUnaryOperand,
                                format!(
                                    "cannot apply unary operator '{:?}' to {}",
                                    op, operand_type
                                ),
                            )
                            .with_label(Label::primary(operand.span, "invalid operand type")),
                        );
                        Type::Error
                    }
                }
            }

            Expr::Call { name, name_span, args } => {
                // Check for built-in println
                if name == "println" {
                    // println accepts any type
                    for arg in args {
                        self.infer_type(arg);
                    }
                    return Type::Unit;
                }

                // Look up function
                if let Some((param_types, return_type)) = self.env.get_function(name).cloned() {
                    // Check argument count
                    if args.len() != param_types.len() {
                        self.diagnostics.emit(
                            Diagnostic::error(
                                ErrorCode::WrongArgumentCount,
                                format!(
                                    "function '{}' expects {} argument(s), found {}",
                                    name,
                                    param_types.len(),
                                    args.len()
                                ),
                            )
                            .with_label(Label::primary(*name_span, "called here")),
                        );
                        return Type::Error;
                    }

                    // Check argument types
                    for (i, (arg, expected_type)) in args.iter().zip(param_types.iter()).enumerate()
                    {
                        let arg_type = self.infer_type(arg);
                        if arg_type != Type::Error
                            && !self.types_compatible(expected_type, &arg_type)
                        {
                            self.diagnostics.emit(
                                Diagnostic::error(
                                    ErrorCode::TypeMismatch,
                                    format!(
                                        "argument {} has type {}, expected {}",
                                        i + 1,
                                        arg_type,
                                        expected_type
                                    ),
                                )
                                .with_label(Label::primary(arg.span, "wrong type")),
                            );
                        }
                    }

                    return_type
                } else {
                    self.diagnostics.emit(
                        Diagnostic::error(
                            ErrorCode::UndefinedFunction,
                            format!("undefined function '{}'", name),
                        )
                        .with_label(Label::primary(*name_span, "not found")),
                    );
                    Type::Error
                }
            }

            Expr::Grouped(inner) => self.infer_type(inner),
        }
    }

    /// Resolve a type annotation to a Type
    fn resolve_type_annotation(&mut self, annotation: &TypeAnnotation) -> Type {
        annotation.ty.clone()
    }

    /// Check if two types are compatible (for assignment, etc.)
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        // Error type is compatible with anything (to avoid cascading errors)
        if *expected == Type::Error || *actual == Type::Error {
            return true;
        }
        expected == actual
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
