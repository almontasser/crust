use crate::{lexer::Token, parser::Symbol, types::Type};

#[derive(Debug, Clone)]
pub enum LiteralValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Identifier(String),
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
    GlobalVar {
        identifier: Token,
        ty: Type,
    },
    GlobalVarMany {
        identifiers: Vec<Token>,
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
        return_type: Option<Type>,
    },
    FnCall {
        identifier: Token,
        expr: Box<Node>,
        ty: Type,
    },
    ReturnStmt {
        expr: Box<Node>,
        fn_name: Symbol,
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
}

impl Node {
    pub fn ty(&self) -> Option<Type> {
        match self {
            Node::BinaryExpr { ty, .. } => Some(ty.clone()),
            Node::UnaryExpr { ty, .. } => Some(ty.clone()),
            Node::WidenExpr { ty, .. } => Some(ty.clone()),
            Node::ScaleExpr { ty, .. } => Some(ty.clone()),
            Node::LiteralExpr { ty, .. } => Some(ty.clone()),
            Node::GlobalVar { ty, .. } => Some(ty.clone()),
            Node::GlobalVarMany { ty, .. } => Some(ty.clone()),
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
        }
    }
}
