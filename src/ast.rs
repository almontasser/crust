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
}
