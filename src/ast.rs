use crate::lexer::{Literal, Token};

#[derive(Debug, Clone)]
pub enum Node {
    BinaryExpr {
        left: Box<Node>,
        operator: Token,
        right: Box<Node>,
    },
    UnaryExpr {
        operator: Token,
        right: Box<Node>,
    },
    LiteralExpr {
        value: Literal,
    },
    PrintStmt {
        expr: Box<Node>,
    },
    GlobalVar {
        identifier: Token,
    },
    AssignStmt {
        identifier: Token,
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
    },
}
