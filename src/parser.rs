use crate::{
    ast::Node,
    lexer::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    nodes: Vec<Node>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            nodes: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> &Vec<Node> {
        while !self.is_at_end() {
            let node = self.addition_expr();
            self.nodes.push(node);
        }

        &self.nodes
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn multiplication_expr(&mut self) -> Node {
        let mut node = self.primary();

        while self.match_token(vec![TokenType::Mul, TokenType::Div]) {
            let operator = self.previous();
            let right = self.primary();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn addition_expr(&mut self) -> Node {
        let mut node = self.multiplication_expr();

        while self.match_token(vec![TokenType::Add, TokenType::Sub]) {
            let operator = self.previous();
            let right = self.multiplication_expr();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn primary(&mut self) -> Node {
        if self.match_token(vec![TokenType::Integer]) {
            return Node::LiteralExpr {
                value: self.previous().value.unwrap(),
            };
        }

        let token = self.peek();
        panic!(
            "Unexpected token {:?} at line {} column {}",
            token.token_type, token.line, token.column
        );
    }

    fn match_token(&mut self, vec: Vec<TokenType>) -> bool {
        for token_type in vec {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token_type == token_type
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
}
