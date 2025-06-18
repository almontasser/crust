use std::{cell::RefCell, rc::Rc};

use crate::{lexer::Token, parser::Symbol, types::Type};

#[derive(Debug, Clone)]
pub enum LiteralValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Identifier(Rc<RefCell<Symbol>>),
    String { value: String, label: String },
}

#[derive(Debug, Clone)]
pub enum Node {
    BinaryExpr {
        left: Box<Node>,
        operator: Token,
        right: Box<Node>,
        ty: Type,
    },
    UnaryExpr {
        operator: Token,
        right: Box<Node>,
        ty: Type,
    },
    WidenExpr {
        right: Box<Node>,
        ty: Type,
    },
    ScaleExpr {
        right: Box<Node>,
        size: usize,
        ty: Type,
    },
    LiteralExpr {
        value: LiteralValue,
        ty: Type,
    },
    VarDecl {
        symbol: Rc<RefCell<Symbol>>,
        is_local: bool,
        ty: Type,
    },
    VarDeclMany {
        symbols: Vec<Rc<RefCell<Symbol>>>,
        is_local: bool,
        ty: Type,
    },
    AssignStmt {
        left: Box<Node>,
        expr: Box<Node>,
    },
    CompoundStmt {
        statements: Vec<Node>,
    },
    IfStmt {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
    },
    WhileStmt {
        condition: Box<Node>,
        body: Box<Node>,
    },
    FnDecl {
        identifier: Token,
        body: Box<Node>,
        stack_size: usize,
        return_type: Option<Type>,
        params: Vec<Rc<RefCell<Symbol>>>,
    },
    FnCall {
        identifier: Token,
        args: Vec<Node>,
        ty: Type,
    },
    ReturnStmt {
        expr: Box<Node>,
        fn_name: Rc<RefCell<Symbol>>,
    },
    PostIncStmt {
        left: Box<Node>,
    },
    PostDecStmt {
        left: Box<Node>,
    },
    PreIncStmt {
        right: Box<Node>,
    },
    PreDecStmt {
        right: Box<Node>,
    },
    ToBool {
        expr: Box<Node>,
    },
    StructDecl {
        identifier: Token,
        fields: Vec<(Token, Type)>,
    },
    UnionDecl {
        identifier: Token,
        fields: Vec<(Token, Type)>,
    },
    EnumDecl {
        identifier: Token,
        variants: Vec<(Token, u64)>,
    },
}

impl Node {
    pub fn ty(&self) -> Option<Type> {
        match self {
            Node::BinaryExpr { ty, .. } => Some(ty.clone()),
            Node::UnaryExpr { ty, .. } => Some(ty.clone()),
            Node::WidenExpr { ty, .. } => Some(ty.clone()),
            Node::ScaleExpr { ty, .. } => Some(ty.clone()),
            Node::LiteralExpr { ty, .. } => Some(ty.clone()),
            Node::VarDecl { ty, .. } => Some(ty.clone()),
            Node::VarDeclMany { ty, .. } => Some(ty.clone()),
            Node::AssignStmt { expr, .. } => expr.ty(),
            Node::CompoundStmt { .. } => None,
            Node::IfStmt { .. } => None,
            Node::WhileStmt { .. } => None,
            Node::FnDecl { .. } => None,
            Node::FnCall { ty, .. } => Some(ty.clone()),
            Node::ReturnStmt { .. } => None,
            Node::PostIncStmt { left } => left.ty(),
            Node::PostDecStmt { left } => left.ty(),
            Node::PreIncStmt { right } => right.ty(),
            Node::PreDecStmt { right } => right.ty(),
            Node::ToBool { .. } => Some(Type::U8),
            Node::StructDecl { .. } => None,
            Node::UnionDecl { .. } => None,
            Node::EnumDecl { .. } => None,
        }
    }
}
