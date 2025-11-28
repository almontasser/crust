//! AST module - Abstract Syntax Tree definitions with span information

use crate::span::Span;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}

pub type SpannedExpr = Spanned<Expr>;
pub type SpannedStmt = Spanned<Stmt>;

/// Type annotation in source code
#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub ty: Type,
    pub span: Span,
}

/// Function parameter with name and type
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub name_span: Span,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Identifier(String),
    BinaryOp {
        left: Box<SpannedExpr>,
        op: BinOp,
        right: Box<SpannedExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<SpannedExpr>,
    },
    Call {
        name: String,
        name_span: Span,
        args: Vec<SpannedExpr>,
    },
    Grouped(Box<SpannedExpr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(SpannedExpr),
    Let {
        name: String,
        name_span: Span,
        type_annotation: Option<TypeAnnotation>,
        value: SpannedExpr,
    },
    Return(Option<SpannedExpr>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub name_span: Span,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Vec<SpannedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}
