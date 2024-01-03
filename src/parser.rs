use core::panic;

use crate::{
    ast::{LiteralValue, Node},
    lexer::{Literal, Token, TokenType},
    types::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Function,
    Variable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Global,
    Local,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub identifier: Token,
    pub structure: SymbolType,
    pub class: StorageClass,
    pub ty: Option<Type>,
    pub end_label: Option<String>,
    pub size: Option<usize>,
    pub offset: Option<isize>,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    nodes: Vec<Node>,
    symbols: Vec<Symbol>,
    current_fn: Option<Symbol>,
    local_offset: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            nodes: Vec::new(),
            symbols: vec![
                // builtin functions
                // add print function
                Symbol {
                    identifier: Token {
                        token_type: TokenType::Identifier,
                        lexeme: Some(String::from("printint")),
                        line: 0,
                        column: 0,
                        value: None,
                    },
                    structure: SymbolType::Function,
                    class: StorageClass::Global,
                    ty: Some(Type::U8),
                    end_label: None,
                    size: None,
                    offset: None,
                },
                Symbol {
                    identifier: Token {
                        token_type: TokenType::Identifier,
                        lexeme: Some(String::from("printchar")),
                        line: 0,
                        column: 0,
                        value: None,
                    },
                    structure: SymbolType::Function,
                    class: StorageClass::Global,
                    ty: Some(Type::U8),
                    end_label: None,
                    size: None,
                    offset: None,
                },
            ],
            current_fn: None,
            local_offset: 0,
        }
    }

    pub fn parse(&mut self) -> &Vec<Node> {
        while !self.is_at_end() {
            let node = if self.match_token(vec![TokenType::Let]) {
                let node = self.var_decl(false);
                self.expect(vec![TokenType::SemiColon]).unwrap();
                node
            } else {
                self.fn_decl()
            };

            self.nodes.push(node);
        }

        &self.nodes
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn compound_statement(&mut self) -> Node {
        let mut nodes = Vec::new();

        self.expect(vec![TokenType::LeftBrace]).unwrap();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let node = self.single_statement();
            match node {
                Node::AssignStmt { .. }
                | Node::VarDecl { .. }
                | Node::VarDeclMany { .. }
                | Node::FnCall { .. }
                | Node::ReturnStmt { .. } => {
                    self.expect(vec![TokenType::SemiColon]).unwrap();
                }
                _ => {}
            }
            nodes.push(node);
        }

        self.expect(vec![TokenType::RightBrace]).unwrap();

        Node::CompoundStmt { statements: nodes }
    }

    fn single_statement(&mut self) -> Node {
        if self.match_token(vec![TokenType::Let]) {
            self.var_decl(true)
        // } else if self.match_token(vec![TokenType::Identifier]) {
        //     self.assignment()
        } else if self.match_token(vec![TokenType::If]) {
            self.if_statement()
        } else if self.match_token(vec![TokenType::While]) {
            self.while_statement()
        } else if self.match_token(vec![TokenType::For]) {
            self.for_statement()
        } else if self.match_token(vec![TokenType::Fn]) {
            self.fn_decl()
        } else if self.match_token(vec![TokenType::Return]) {
            self.return_statement()
        } else {
            self.expression()
        }
    }

    fn parse_type(&mut self) -> Type {
        // a type of a variable is like these examples:
        // let x: int;
        // let y: u8;
        // let z: *u32; // pointer to u32
        // let a: **int; // pointer to pointer to int

        let mut pointers_counter: u8 = 0;
        while self.match_token(vec![TokenType::Mul]) {
            pointers_counter += 1
        }

        let ty_token = self
            .expect(vec![
                TokenType::U8,
                TokenType::U16,
                TokenType::U32,
                TokenType::U64,
                TokenType::I8,
                TokenType::I16,
                TokenType::I32,
                TokenType::I64,
                TokenType::Char,
            ])
            .unwrap();

        let (is_array, size) = if self.match_token(vec![TokenType::LeftBracket]) {
            let size_token = self.expect(vec![TokenType::Integer]).unwrap();
            let size = match size_token.value {
                Some(Literal::Integer(val)) => val,
                _ => panic!("Expected integer"),
            };
            self.expect(vec![TokenType::RightBracket]).unwrap();
            (true, size)
        } else {
            (false, 0)
        };

        let mut ty = match ty_token.token_type {
            TokenType::U8 => Type::U8,
            TokenType::U16 => Type::U16,
            TokenType::U32 => Type::U32,
            TokenType::U64 => Type::U64,
            TokenType::I8 => Type::I8,
            TokenType::I16 => Type::I16,
            TokenType::I32 => Type::I32,
            TokenType::I64 => Type::I64,
            TokenType::Char => Type::Char,
            _ => panic!("Expected type"),
        };

        for _ in 0..pointers_counter {
            ty = ty.pointer_to();
        }

        if is_array {
            ty = Type::Array {
                ty: Box::new(ty),
                count: size,
            };
        }

        ty
    }

    fn var_decl(&mut self, is_local: bool) -> Node {
        let mut identifiers = Vec::new();
        let class = if is_local {
            StorageClass::Local
        } else {
            StorageClass::Global
        };

        while self.match_token(vec![TokenType::Identifier]) {
            identifiers.push(self.previous(1));

            if !self.match_token(vec![TokenType::Comma]) {
                break;
            }
        }
        self.expect(vec![TokenType::Colon]).unwrap();
        let ty = self.parse_type();

        if identifiers.clone().len() == 1 {
            let offset = if is_local {
                Some(self.gen_offset(ty.clone()))
            } else {
                None
            };

            let structure = SymbolType::Variable;
            let symbol = self.add_symbol(
                identifiers[0].clone(),
                structure,
                class.clone(),
                Some(ty.clone()),
                None,
                offset,
            );

            Node::VarDecl {
                symbol,
                is_local,
                ty: ty.clone(),
            }
        } else {
            let mut symbols = Vec::new();
            for identifier in &identifiers {
                let offset = if is_local {
                    Some(self.gen_offset(ty.clone()))
                } else {
                    None
                };

                symbols.push(self.add_symbol(
                    identifier.clone(),
                    SymbolType::Variable,
                    class.clone(),
                    Some(ty.clone()),
                    None,
                    offset,
                ));
            }

            Node::VarDeclMany {
                symbols,
                is_local,
                ty: ty.clone(),
            }
        }
    }

    fn if_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]).unwrap();
        let mut expr = self.expression();
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
            // _ => panic!(
            //     "Expected comparison operator but got {:?} at line {} column {}",
            //     expr,
            //     self.previous(1).line,
            //     self.previous(1).column
            // ),
            _ => {
                expr = Node::ToBool {
                    expr: Box::new(expr),
                };
            }
        }
        self.expect(vec![TokenType::RightParen]).unwrap();
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

    fn expression(&mut self) -> Node {
        self.logical_expr()
    }

    fn logical_expr(&mut self) -> Node {
        let mut node = self.bitwise_expr();

        while self.match_token(vec![TokenType::LogicalAnd, TokenType::LogicalOr]) {
            let operator = self.previous(1);
            let right = self.bitwise_expr();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
                ty: Type::U8,
            };
        }

        node
    }

    fn bitwise_expr(&mut self) -> Node {
        let mut node = self.equality();

        while self.match_token(vec![TokenType::Or, TokenType::Xor, TokenType::Ampersand]) {
            let operator = self.previous(1);
            let right = self.equality();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
                ty: Type::U8,
            };
        }

        node
    }

    fn equality(&mut self) -> Node {
        let mut node = self.comparison();

        while self.match_token(vec![TokenType::Equal, TokenType::NotEqual]) {
            let operator = self.previous(1);
            let right = self.comparison();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
                ty: Type::U8,
            };
        }

        node
    }

    fn comparison(&mut self) -> Node {
        let mut node = self.shift();

        while self.match_token(vec![
            TokenType::LessThan,
            TokenType::LessThanOrEqual,
            TokenType::GreaterThan,
            TokenType::GreaterThanOrEqual,
        ]) {
            let operator = self.previous(1);
            let right = self.shift();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
                ty: Type::U8,
            };
        }

        node
    }

    fn shift(&mut self) -> Node {
        let mut node = self.term();

        while self.match_token(vec![TokenType::LeftShift, TokenType::RightShift]) {
            let operator = self.previous(1);
            let right = self.term();
            node = Node::BinaryExpr {
                left: Box::new(node),
                operator,
                right: Box::new(right),
                ty: Type::U8,
            };
        }

        node
    }

    fn term(&mut self) -> Node {
        let mut left = self.factor();

        while self.match_token(vec![TokenType::Add, TokenType::Sub]) {
            let operator = self.previous(1);
            let mut right = self.factor();

            let temp_left =
                self.modify_type(left.clone(), right.ty().unwrap(), Some(operator.token_type));

            let temp_right =
                self.modify_type(right.clone(), left.ty().unwrap(), Some(operator.token_type));

            if temp_left.is_none() && temp_right.is_none() {
                panic!(
                    "Incompatible types at line {} column {}",
                    operator.line, operator.column
                );
            }

            if temp_left.is_some() {
                left = temp_left.unwrap();
            }

            if temp_right.is_some() {
                right = temp_right.unwrap();
            }

            left = Node::BinaryExpr {
                left: Box::new(left.clone()),
                operator,
                right: Box::new(right),
                ty: left.ty().unwrap(),
            };
        }

        left
    }

    fn factor(&mut self) -> Node {
        let mut left = self.unary();

        while self.match_token(vec![TokenType::Mul, TokenType::Div]) {
            let operator = self.previous(1);
            let mut right = self.unary();

            let temp_left =
                self.modify_type(left.clone(), right.ty().unwrap(), Some(operator.token_type));

            let temp_right =
                self.modify_type(right.clone(), left.ty().unwrap(), Some(operator.token_type));

            if temp_left.is_none() && temp_right.is_none() {
                panic!(
                    "Incompatible types at line {} column {}",
                    operator.line, operator.column
                );
            }

            if temp_left.is_some() {
                left = temp_left.unwrap();
            }

            if temp_right.is_some() {
                right = temp_right.unwrap();
            }

            left = Node::BinaryExpr {
                left: Box::new(left.clone()),
                operator,
                right: Box::new(right),
                ty: left.ty().unwrap(),
            };
        }

        left
    }

    fn unary(&mut self) -> Node {
        if self.match_token(vec![
            TokenType::Sub,
            TokenType::LogicalNot,
            TokenType::Invert,
        ]) {
            let operator = self.previous(1);
            let right = self.unary();
            return Node::UnaryExpr {
                operator,
                right: Box::new(right.clone()),
                ty: right.ty().unwrap(),
            };
        }

        self.prefix()
    }

    fn prefix(&mut self) -> Node {
        let mut node: Node;
        if self.match_token(vec![TokenType::Ampersand]) {
            node = self.prefix();

            // ensure that the node is an identifier
            match &node {
                Node::LiteralExpr {
                    value: LiteralValue::Identifier(_),
                    ..
                } => {}
                _ => panic!("Expected identifier"),
            }

            node = Node::UnaryExpr {
                operator: Token {
                    token_type: TokenType::Ampersand,
                    lexeme: None,
                    line: self.previous(1).line,
                    column: self.previous(1).column,
                    value: None,
                },
                right: Box::new(node.clone()),
                ty: node.ty().unwrap().pointer_to(),
            };
        } else if self.match_token(vec![TokenType::Mul]) {
            node = self.prefix();

            // ensure that the node is an identifier or a dereference
            match &node {
                Node::LiteralExpr { value, .. } => match value {
                    LiteralValue::Identifier(_) => {}
                    _ => panic!("Expected identifier"),
                },
                Node::UnaryExpr { operator, .. } => {
                    if operator.token_type != TokenType::Ampersand {
                        panic!("Expected identifier");
                    }
                }
                Node::AssignStmt { left, expr } => {
                    return Node::AssignStmt {
                        left: Box::new(Node::UnaryExpr {
                            operator: Token {
                                token_type: TokenType::Mul,
                                lexeme: None,
                                line: self.previous(1).line,
                                column: self.previous(1).column,
                                value: None,
                            },
                            right: left.clone(),
                            ty: left.ty().unwrap(),
                        }),
                        expr: expr.clone(),
                    };
                }
                _ => panic!(
                    "Expected identifier at line {} column {}, got {:?}",
                    self.previous(1).line,
                    self.previous(1).column,
                    node
                ),
            }

            node = Node::UnaryExpr {
                operator: Token {
                    token_type: TokenType::Mul,
                    lexeme: None,
                    line: self.previous(1).line,
                    column: self.previous(1).column,
                    value: None,
                },
                right: Box::new(node.clone()),
                ty: node.ty().unwrap().value_at(),
            };
        } else if self.match_token(vec![TokenType::Inc]) {
            node = self.prefix();

            // ensure that the node is an identifier
            match &node {
                Node::LiteralExpr { value, .. } => match value {
                    LiteralValue::Identifier(_) => {}
                    _ => panic!("Expected identifier"),
                },
                _ => panic!("Expected identifier"),
            }

            node = Node::PreIncStmt {
                right: Box::new(node),
            };
        } else if self.match_token(vec![TokenType::Dec]) {
            node = self.prefix();

            // ensure that the node is an identifier
            match &node {
                Node::LiteralExpr {
                    value: LiteralValue::Identifier(_),
                    ..
                } => {}
                _ => panic!("Expected identifier"),
            }

            node = Node::PreDecStmt {
                right: Box::new(node),
            };
        } else {
            node = self.primary();
        }

        node
    }

    fn postfix(&mut self) -> Node {
        let identifier = self.previous(1);
        match self.find_symbol(identifier.clone()) {
            Some(symbol) => {
                // TODO: This is hacky, fix it
                if self.match_token(vec![TokenType::LeftParen]) {
                    if symbol.structure != SymbolType::Function {
                        panic!(
                            "Expected function at line {} column {}",
                            identifier.line, identifier.column
                        );
                    }
                    return self.function_call();
                } else if symbol.structure != SymbolType::Variable {
                    panic!(
                        "Expected variable at line {} column {} got {:?}",
                        identifier.line, identifier.column, symbol.structure
                    );
                }

                let left = if self.match_token(vec![TokenType::LeftBracket]) {
                    self.array_access()
                } else {
                    let symbol = self.find_symbol(identifier.clone()).unwrap_or_else(|| {
                        panic!(
                            "Variable {} not declared at line {} column {}",
                            identifier.lexeme.clone().unwrap(),
                            identifier.line,
                            identifier.column
                        )
                    });

                    Node::LiteralExpr {
                        value: LiteralValue::Identifier(symbol.clone()),
                        ty: symbol.ty.unwrap(),
                    }
                };

                if self.match_token(vec![TokenType::Assign]) {
                    let expr = self.expression();

                    // expr = match self.modify_type(expr, symbol.ty.unwrap(), None) {
                    //     Some(node) => node,
                    //     None => panic!(
                    //         "Incompatible types at line {} column {}",
                    //         self.previous(1).line,
                    //         self.previous(1).column
                    //     ),
                    // };

                    Node::AssignStmt {
                        left: Box::new(left),
                        expr: Box::new(expr),
                    }
                } else if self.match_token(vec![TokenType::Inc]) {
                    Node::PostIncStmt {
                        left: Box::new(left),
                    }
                } else if self.match_token(vec![TokenType::Dec]) {
                    Node::PostDecStmt {
                        left: Box::new(left),
                    }
                } else {
                    left
                }
            }
            None => panic!(
                "Variable {} not declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.column
            ),
        }
    }

    fn primary(&mut self) -> Node {
        if self.match_token(vec![TokenType::LeftParen]) {
            let expr = self.expression();
            self.expect(vec![TokenType::RightParen]).unwrap();
            return expr;
        } else if self.match_token(vec![TokenType::Integer]) {
            let val: u64 = match self.previous(1).value {
                Some(Literal::Integer(val)) => val,
                _ => panic!("Expected integer"),
            };
            let (value, ty) = if val <= u8::MAX as u64 {
                (LiteralValue::U8(val as u8), Type::U8)
            } else if val <= u16::MAX as u64 {
                (LiteralValue::U16(val as u16), Type::U16)
            } else if val <= u32::MAX as u64 {
                (LiteralValue::U32(val as u32), Type::U32)
            } else {
                (LiteralValue::U64(val), Type::U64)
            };
            return Node::LiteralExpr { value, ty };
        } else if self.match_token(vec![TokenType::Identifier]) {
            return self.postfix();
        } else if self.match_token(vec![TokenType::String]) {
            let val = match self.previous(1).value {
                Some(Literal::String { value, .. }) => value,
                _ => panic!("Expected string"),
            };

            let ty = Type::Array {
                ty: Box::new(Type::U8),
                count: val.len() as u64,
            };
            let label = match self.previous(1).value {
                Some(Literal::String { label, .. }) => label,
                _ => panic!("Expected string"),
            };
            return Node::LiteralExpr {
                value: LiteralValue::String { value: val, label },
                ty,
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

    fn expect(&mut self, tokens: Vec<TokenType>) -> Result<Token, String> {
        for token in &tokens {
            if self.check(*token) {
                return Ok(self.advance());
            }
        }

        Err(format!(
            "Expected {:?} at line {} column {}, got {:?}",
            tokens,
            self.peek().line,
            self.peek().column,
            self.peek().token_type
        ))
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

        self.previous(1)
    }

    fn previous(&self, i: usize) -> Token {
        self.tokens[self.current - i].clone()
    }

    fn add_symbol(
        &mut self,
        identifier: Token,
        structure: SymbolType,
        class: StorageClass,
        ty: Option<Type>,
        end_label: Option<String>,
        offset: Option<isize>,
    ) -> Symbol {
        let symbol = self.find_symbol(identifier.clone());
        if symbol.is_some() {
            panic!(
                "Variable {} already declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.column
            );
        }

        let symbol = Symbol {
            identifier,
            structure,
            class,
            ty,
            end_label,
            size: None,
            offset,
        };

        self.symbols.push(symbol.clone());

        symbol
    }

    fn find_symbol(&self, identifier: Token) -> Option<Symbol> {
        let mut symbol: Option<Symbol> = None;
        for it in &self.symbols {
            if it.identifier.lexeme.clone().unwrap() == identifier.lexeme.clone().unwrap() {
                symbol = Some(it.clone());
                if it.class == StorageClass::Local {
                    break;
                }
            }
        }

        symbol
    }

    fn while_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]).unwrap();
        let mut expr = self.expression();
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
            // _ => panic!("Expected comparison operator"),
            _ => {
                expr = Node::ToBool {
                    expr: Box::new(expr),
                };
            }
        }
        self.expect(vec![TokenType::RightParen]).unwrap();
        let body = self.compound_statement();

        Node::WhileStmt {
            condition: Box::new(expr),
            body: Box::new(body),
        }
    }

    fn for_statement(&mut self) -> Node {
        self.expect(vec![TokenType::LeftParen]).unwrap();
        let initializer = if self.match_token(vec![TokenType::SemiColon]) {
            None
        // } else if self.match_token(vec![TokenType::Let]) {
        //     Some(self.var_decl())
        } else if self.check(TokenType::Identifier) {
            let node = self.expression();
            self.expect(vec![TokenType::SemiColon]).unwrap();
            Some(node)
        } else {
            panic!("Expected identifier");
        };

        let condition = if self.check(TokenType::SemiColon) {
            Node::LiteralExpr {
                value: LiteralValue::U8(1),
                ty: Type::U8,
            }
        } else {
            let mut expr = self.expression();
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
                // _ => panic!("Expected comparison operator"),
                _ => {
                    expr = Node::ToBool {
                        expr: Box::new(expr),
                    };
                }
            }
            expr
        };
        self.expect(vec![TokenType::SemiColon]).unwrap();

        let increment = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.single_statement())
        };
        self.expect(vec![TokenType::RightParen]).unwrap();

        let mut body = self.compound_statement();

        if let Some(increment) = increment {
            body = Node::CompoundStmt {
                statements: vec![body, increment],
            };
        }

        body = Node::WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
        };

        if let Some(initializer) = initializer {
            body = Node::CompoundStmt {
                statements: vec![initializer, body],
            };
        }

        body
    }

    fn fn_decl(&mut self) -> Node {
        self.expect(vec![TokenType::Fn]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        let end_label = Some(format!("{}{}", identifier.lexeme.clone().unwrap(), "_end"));
        self.expect(vec![TokenType::LeftParen]).unwrap();
        // TODO: parse parameters
        self.expect(vec![TokenType::RightParen]).unwrap();

        let mut ty: Option<Type> = None;
        if self.match_token(vec![TokenType::Colon]) {
            ty = Some(self.parse_type());
        }
        let symbol = self.add_symbol(
            identifier.clone(),
            SymbolType::Function,
            StorageClass::Global,
            ty.clone(),
            end_label,
            None,
        );
        self.current_fn = Some(symbol.clone());
        self.reset_offset();
        let body = self.compound_statement();
        // ensure that the function returns a value if it has a return type in the last statement
        if ty.is_some() {
            match &body {
                Node::CompoundStmt { statements } => {
                    if statements.is_empty() {
                        panic!(
                            "Function {} does not return a value at line {} column {}",
                            identifier.lexeme.clone().unwrap(),
                            identifier.line,
                            identifier.column
                        );
                    }

                    let last = statements.last().unwrap();
                    match last {
                        Node::ReturnStmt { .. } => {}
                        _ => panic!(
                            "Function {} does not return a value at line {} column {}",
                            identifier.lexeme.clone().unwrap(),
                            identifier.line,
                            identifier.column
                        ),
                    }
                }
                _ => panic!(
                    "Function {} does not return a value at line {} column {}",
                    identifier.lexeme.clone().unwrap(),
                    identifier.line,
                    identifier.column
                ),
            }
        }

        self.current_fn = None;

        Node::FnDecl {
            identifier,
            body: Box::new(body),
            stack_size: self.local_offset,
            return_type: ty,
        }
    }

    fn modify_type(&self, node: Node, right_type: Type, op: Option<TokenType>) -> Option<Node> {
        let left_type = node.ty().unwrap();

        if left_type.is_int() && right_type.is_int() {
            if left_type == right_type {
                return Some(node);
            }

            let left_size = left_type.size();
            let right_size = right_type.size();

            if left_size > right_size {
                return None;
            }

            if right_size > left_size {
                return Some(Node::WidenExpr {
                    right: Box::new(node),
                    ty: right_type,
                });
            }
        }

        if left_type.is_ptr() && op.is_none() && left_type == right_type {
            return Some(node);
        }

        // We can scale only on A_ADD or A_SUBTRACT operation
        if op.is_some() && (op == Some(TokenType::Add) || op == Some(TokenType::Sub)) {
            if left_type.is_int() && right_type.is_ptr() {
                let right_size = right_type.value_at().size();
                if right_size > 1 {
                    return Some(Node::ScaleExpr {
                        right: Box::new(node),
                        size: right_size,
                        ty: right_type,
                    });
                } else {
                    return Some(node);
                }
            }
        }

        None
    }

    fn function_call(&mut self) -> Node {
        let identifier = self.previous(2);
        let symbol = self.find_symbol(identifier.clone());

        if symbol.is_none() {
            panic!(
                "Function {} not declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.column
            );
        }

        let symbol = symbol.unwrap();
        if symbol.structure != SymbolType::Function {
            panic!(
                "Expected function at line {} column {}",
                identifier.line, identifier.column
            );
        }

        let expr = self.expression();

        self.expect(vec![TokenType::RightParen]).unwrap();

        Node::FnCall {
            identifier,
            expr: Box::new(expr),
            ty: symbol.ty.unwrap(),
        }
    }

    fn return_statement(&mut self) -> Node {
        if self.current_fn.is_none() {
            panic!("Return statement outside of function");
        }

        let fn_sym = self.current_fn.clone().unwrap();

        if fn_sym.ty.is_none() {
            panic!(
                "Function {} has no return type",
                fn_sym.identifier.lexeme.clone().unwrap()
            );
        }

        let mut expr = self.expression();

        expr = match self.modify_type(expr, fn_sym.clone().ty.unwrap(), None) {
            Some(node) => node,
            None => panic!(
                "Incompatible types at line {} column {}",
                self.previous(1).line,
                self.previous(1).column
            ),
        };

        Node::ReturnStmt {
            expr: Box::new(expr),
            fn_name: fn_sym,
        }
    }

    fn array_access(&mut self) -> Node {
        let identifier = self.previous(2);
        let symbol = self.find_symbol(identifier.clone());

        if symbol.is_none() {
            panic!(
                "Variable {} not declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.column
            );
        }

        let symbol = symbol.unwrap();
        if symbol.structure != SymbolType::Variable {
            panic!(
                "Expected variable at line {} column {} got {:?}",
                identifier.line, identifier.column, symbol.structure
            );
        }

        let symbol = self.find_symbol(identifier.clone()).unwrap_or_else(|| {
            panic!(
                "Variable {} not declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.column
            )
        });
        let mut left = Node::LiteralExpr {
            value: LiteralValue::Identifier(symbol.clone()),
            ty: symbol.ty.unwrap(),
        };

        let mut index = self.expression();

        self.expect(vec![TokenType::RightBracket]).unwrap();

        if !index.ty().unwrap().is_int() {
            panic!(
                "Expected integer at line {} column {}",
                self.previous(1).line,
                self.previous(1).column
            );
        }

        index = match self.modify_type(index, Type::U32, Some(TokenType::Add)) {
            Some(node) => node,
            None => panic!(
                "Incompatible types at line {} column {}",
                self.previous(1).line,
                self.previous(1).column
            ),
        };

        left = Node::BinaryExpr {
            left: Box::new(left.clone()),
            operator: Token {
                token_type: TokenType::Add,
                lexeme: None,
                line: self.previous(1).line,
                column: self.previous(1).column,
                value: None,
            },
            right: Box::new(index),
            ty: left.ty().unwrap(),
        };

        Node::UnaryExpr {
            operator: Token {
                token_type: TokenType::Mul,
                lexeme: None,
                line: self.previous(1).line,
                column: self.previous(1).column,
                value: None,
            },
            right: Box::new(left.clone()),
            ty: left.ty().unwrap(),
        }
    }

    fn gen_offset(&mut self, ty: Type) -> isize {
        let size = ty.size().max(4);
        self.local_offset += size;
        -(self.local_offset as isize)
    }

    fn reset_offset(&mut self) {
        self.local_offset = 0;
    }
}
