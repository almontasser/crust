use core::panic;

use crate::{
    ast::Node,
    lexer::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    nodes: Vec<Node>,
    global_variables: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            nodes: Vec::new(),
            global_variables: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> &Vec<Node> {
        while !self.is_at_end() {
            let node = self.compound_statement();
            self.nodes.push(node);
        }

        &self.nodes
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn compound_statement(&mut self) -> Node {
        let mut nodes = Vec::new();

        self.expect(vec![TokenType::LeftBrace]);

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let node = self.statement();
            nodes.push(node);
        }

        self.expect(vec![TokenType::RightBrace]);

        Node::CompoundStmt { statements: nodes }
    }

    fn statement(&mut self) -> Node {
        // expect print
        if self.match_token(vec![TokenType::Print]) {
            return self.print_statement();
        } else if self.match_token(vec![TokenType::Let]) {
            return self.var_decl();
        } else if self.match_token(vec![TokenType::Identifier]) {
            return self.assignment();
        } else if self.match_token(vec![TokenType::If]) {
            return self.if_statement();
        } else if self.match_token(vec![TokenType::While]) {
            return self.while_statement();
        } else {
            panic!(
                "Expected print at line {} column {}",
                self.peek().line,
                self.peek().column
            );
        }
    }

    fn var_decl(&mut self) -> Node {
        let identifier = self.expect(vec![TokenType::Identifier]);
        self.expect(vec![TokenType::SemiColon]);
        self.add_global_variable(identifier.clone());
        Node::GlobalVar { identifier }
    }

    fn assignment(&mut self) -> Node {
        let identifier = self.previous();
        // make sure the identifier is declared
        let ident_lexeme = identifier.lexeme.clone().unwrap();
        if !self.global_variables.contains(&ident_lexeme) {
            panic!(
                "Variable {} not declared at line {} column {}",
                ident_lexeme, identifier.line, identifier.column
            );
        }
        self.expect(vec![TokenType::Assign]);
        let expr = self.expression();
        self.expect(vec![TokenType::SemiColon]);
        Node::AssignStmt {
            identifier,
            expr: Box::new(expr),
        }
    }

    fn if_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]);
        let expr = self.expression();
        match &expr {
            Node::BinaryExpr { operator, .. } => {
                if operator.token_type != TokenType::Equal
                    && operator.token_type != TokenType::NotEqual
                    && operator.token_type != TokenType::LessThan
                    && operator.token_type != TokenType::LessThanOrEqual
                    && operator.token_type != TokenType::GreaterThan
                    && operator.token_type != TokenType::GreaterThanOrEqual
                {
                    panic!(
                        "Expected comparison operator at line {} column {}",
                        operator.line, operator.column
                    );
                }
            }
            _ => panic!("Expected comparison operator"),
        }
        self.expect(vec![TokenType::RightParen]);
        let then_branch = self.compound_statement();
        let else_branch = if self.match_token(vec![TokenType::Else]) {
            Some(Box::new(self.compound_statement()))
        } else {
            None
        };

        Node::IfStmt {
            condition: Box::new(expr),
            then_branch: Box::new(then_branch),
            else_branch,
        }
    }

    fn print_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]);
        let expr = self.expression();
        self.expect(vec![TokenType::RightParen]);
        self.expect(vec![TokenType::SemiColon]);
        Node::PrintStmt {
            expr: Box::new(expr),
        }
    }

    fn expression(&mut self) -> Node {
        let node = self.equality();
        node
    }

    fn equality(&mut self) -> Node {
        let mut node = self.comparison();

        while self.match_token(vec![TokenType::Equal, TokenType::NotEqual]) {
            let operator = self.previous();
            let right = self.comparison();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn comparison(&mut self) -> Node {
        let mut node = self.term();

        while self.match_token(vec![
            TokenType::LessThan,
            TokenType::LessThanOrEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanOrEqual,
        ]) {
            let operator = self.previous();
            let right = self.term();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn term(&mut self) -> Node {
        let mut node = self.factor();

        while self.match_token(vec![TokenType::Add, TokenType::Sub]) {
            let operator = self.previous();
            let right = self.factor();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn factor(&mut self) -> Node {
        let mut node = self.unary();

        while self.match_token(vec![TokenType::Mul, TokenType::Div]) {
            let operator = self.previous();
            let right = self.unary();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
            };
        }

        node
    }

    fn unary(&mut self) -> Node {
        if self.match_token(vec![TokenType::Sub]) {
            let operator = self.previous();
            let right = self.unary();
            return Node::UnaryExpr {
                operator,
                right: Box::new(right),
            };
        }

        self.primary()
    }

    fn primary(&mut self) -> Node {
        if self.match_token(vec![TokenType::Integer]) {
            return Node::LiteralExpr {
                value: self.previous().value.unwrap(),
            };
        } else if self.match_token(vec![TokenType::Identifier]) {
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

    fn expect(&mut self, tokens: Vec<TokenType>) -> Token {
        for token in &tokens {
            if self.check(*token) {
                return self.advance();
            }
        }

        panic!(
            "Expected {:?} at line {} column {}, got {:?}",
            tokens,
            self.peek().line,
            self.peek().column,
            self.peek().token_type
        );
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

    fn add_global_variable(&mut self, identifier: Token) {
        if let Some(lexeme) = identifier.lexeme.clone() {
            if !self.global_variables.contains(&lexeme) {
                self.global_variables.push(lexeme);
            }
        }
    }

    fn while_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]);
        let expr = self.expression();
        match &expr {
            Node::BinaryExpr { operator, .. } => {
                if operator.token_type != TokenType::Equal
                    && operator.token_type != TokenType::NotEqual
                    && operator.token_type != TokenType::LessThan
                    && operator.token_type != TokenType::LessThanOrEqual
                    && operator.token_type != TokenType::GreaterThan
                    && operator.token_type != TokenType::GreaterThanOrEqual
                {
                    panic!(
                        "Expected comparison operator at line {} column {}",
                        operator.line, operator.column
                    );
                }
            }
            _ => panic!("Expected comparison operator"),
        }
        self.expect(vec![TokenType::RightParen]);
        let body = self.compound_statement();

        Node::WhileStmt {
            condition: Box::new(expr),
            body: Box::new(body),
        }
    }
}
