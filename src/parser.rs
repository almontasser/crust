use core::panic;
use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{LiteralValue, Node},
    lexer::{Literal, Token, TokenType},
    types::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Function,
    Variable,
    StructType,
    UnionType,
    EnumType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Global,
    Local,
    Param,
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
    pub params: Option<Vec<Rc<RefCell<Symbol>>>>,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    nodes: Vec<Node>,
    symbols: Vec<Rc<RefCell<Symbol>>>,
    current_fn: Option<Rc<RefCell<Symbol>>>,
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
                Rc::new(RefCell::new(Symbol {
                    identifier: Token {
                        token_type: TokenType::Identifier,
                        lexeme: Some(String::from("printint")),
                        line: 0,
                        start_column: 0,
                        end_column: 0,
                        value: None,
                    },
                    structure: SymbolType::Function,
                    class: StorageClass::Global,
                    ty: Some(Type::U8),
                    end_label: None,
                    size: None,
                    offset: None,
                    params: Some(vec![Rc::new(RefCell::new(Symbol {
                        identifier: Token {
                            token_type: TokenType::Identifier,
                            lexeme: Some(String::from("x")),
                            line: 0,
                            start_column: 0,
                            end_column: 0,
                            value: None,
                        },
                        structure: SymbolType::Variable,
                        class: StorageClass::Param,
                        ty: Some(Type::U8),
                        end_label: None,
                        size: None,
                        offset: None,
                        params: None,
                    }))]),
                })),
                Rc::new(RefCell::new(Symbol {
                    identifier: Token {
                        token_type: TokenType::Identifier,
                        lexeme: Some(String::from("printchar")),
                        line: 0,
                        start_column: 0,
                        end_column: 0,
                        value: None,
                    },
                    structure: SymbolType::Function,
                    class: StorageClass::Global,
                    ty: Some(Type::U8),
                    end_label: None,
                    size: None,
                    offset: None,
                    params: Some(vec![Rc::new(RefCell::new(Symbol {
                        identifier: Token {
                            token_type: TokenType::Identifier,
                            lexeme: Some(String::from("x")),
                            line: 0,
                            start_column: 0,
                            end_column: 0,
                            value: None,
                        },
                        structure: SymbolType::Variable,
                        class: StorageClass::Param,
                        ty: Some(Type::U8),
                        end_label: None,
                        size: None,
                        offset: None,
                        params: None,
                    }))]),
                })),
            ],
            current_fn: None,
            local_offset: 0,
        }
    }

    pub fn parse(&mut self) -> &Vec<Node> {
        // first pass
        while !self.is_at_end() {
            if self.check(TokenType::Extern) {
                self.extern_fn_decl(true);
            } else if self.check(TokenType::Struct) {
                let node = self.parse_struct_decl();
                self.nodes.push(node);
            } else if self.check(TokenType::Union) {
                let node = self.parse_union_decl();
                self.nodes.push(node);
            } else if self.check(TokenType::Enum) {
                let node = self.parse_enum_decl();
                self.nodes.push(node);
            } else if self.match_token(vec![TokenType::Let]) {
                let node = self.var_decl(false);
                self.expect(vec![TokenType::SemiColon]).unwrap();
                self.nodes.push(node);
            } else if self.check(TokenType::Fn) {
                self.fn_decl(true);
            } else {
                self.advance();
            }
        }

        // second pass
        self.current = 0;
        while !self.is_at_end() {
            // skip global variables and type declarations since we already parsed it in the first pass
            if self.check(TokenType::Extern) {
                self.extern_fn_decl(false);
                continue;
            } else if self.check(TokenType::Struct) || self.check(TokenType::Union) || self.check(TokenType::Enum) {
                // Skip the declaration as it's already processed.
                // We need to advance past it. This is a bit naive; a better way would be to store
                // the end position of the declaration or re-parse it without adding to nodes/symbols.
                // For now, let's assume they are top-level and followed by ';'.
                // This will need more robust handling if they can be nested or complex.
                let mut depth = 0;
                loop {
                    if self.check(TokenType::LeftBrace) {
                        depth += 1;
                    } else if self.check(TokenType::RightBrace) {
                        depth -= 1;
                    }
                    self.advance();
                    if depth == 0 && self.check(TokenType::SemiColon) {
                        self.advance();
                        break;
                    }
                    if self.is_at_end() { break; }
                }
                continue;
            } else if self.match_token(vec![TokenType::Let]) {
                // Also skip global variable declarations
                while !self.match_token(vec![TokenType::SemiColon]) {
                    if self.is_at_end() { break; }
                    self.advance();
                }
                continue;
            }


            if self.check(TokenType::Fn) {
                 let node = self.fn_decl(false).unwrap();
                 self.nodes.push(node);
            } else if !self.is_at_end() {
                // If there's anything else, it's unexpected at global scope in second pass
                // unless it's EOF.
                if self.peek().token_type != TokenType::Eof {
                     eprintln!(
                        "Unexpected token {:?} at global scope during second pass at line {} column {}",
                        self.peek().token_type,
                        self.peek().line,
                        self.peek().start_column
                    );
                    // Consume the token to prevent infinite loops on errors
                    self.advance();
                }
            }
        }

        &self.nodes
    }

    fn parse_struct_decl(&mut self) -> Node {
        self.expect(vec![TokenType::Struct]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        self.expect(vec![TokenType::LeftBrace]).unwrap();

        let mut fields = Vec::new();
        let mut struct_size = 0;

        while !self.check(TokenType::RightBrace) {
            let field_name = self.expect(vec![TokenType::Identifier]).unwrap();
            self.expect(vec![TokenType::Colon]).unwrap();
            let field_type = self.parse_type();
            self.expect(vec![TokenType::SemiColon]).unwrap();

            struct_size += field_type.size();
            fields.push((field_name, field_type.clone()));
        }

        self.expect(vec![TokenType::RightBrace]).unwrap();
        self.expect(vec![TokenType::SemiColon]).unwrap();

        let struct_type = Type::Struct {
            name: identifier.lexeme.clone().unwrap(),
            fields: fields
                .iter()
                .map(|(name, ty)| (name.lexeme.clone().unwrap(), ty.clone()))
                .collect(),
            size: struct_size,
        };

        // TODO: Add to symbol table correctly
        self.add_symbol(
            identifier.clone(),
            SymbolType::StructType, // Placeholder
            StorageClass::Global,    // Assuming global for now
            Some(struct_type),
            None,
            None,
            None,
        );

        Node::StructDecl {
            identifier,
            fields,
        }
    }

    fn parse_union_decl(&mut self) -> Node {
        self.expect(vec![TokenType::Union]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        self.expect(vec![TokenType::LeftBrace]).unwrap();

        let mut fields = Vec::new();
        let mut max_field_size = 0;

        while !self.check(TokenType::RightBrace) {
            let field_name = self.expect(vec![TokenType::Identifier]).unwrap();
            self.expect(vec![TokenType::Colon]).unwrap();
            let field_type = self.parse_type();
            self.expect(vec![TokenType::SemiColon]).unwrap();

            if field_type.size() > max_field_size {
                max_field_size = field_type.size();
            }
            fields.push((field_name, field_type.clone()));
        }

        self.expect(vec![TokenType::RightBrace]).unwrap();
        self.expect(vec![TokenType::SemiColon]).unwrap();

        let union_type = Type::Union {
            name: identifier.lexeme.clone().unwrap(),
            fields: fields
                .iter()
                .map(|(name, ty)| (name.lexeme.clone().unwrap(), ty.clone()))
                .collect(),
            size: max_field_size,
        };

        // TODO: Add to symbol table correctly
        self.add_symbol(
            identifier.clone(),
            SymbolType::UnionType, // Placeholder
            StorageClass::Global,   // Assuming global for now
            Some(union_type),
            None,
            None,
            None,
        );

        Node::UnionDecl {
            identifier,
            fields,
        }
    }

    fn parse_enum_decl(&mut self) -> Node {
        self.expect(vec![TokenType::Enum]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        self.expect(vec![TokenType::LeftBrace]).unwrap();

        let mut variants = Vec::new();
        let mut current_value: i64 = 0;

        while !self.check(TokenType::RightBrace) {
            let variant_name = self.expect(vec![TokenType::Identifier]).unwrap();
            let mut literal_value = None;
            let assigned_value;

            if self.match_token(vec![TokenType::Assign]) {
                let value_token = self.expect(vec![TokenType::Integer]).unwrap();
                let val = match value_token.value {
                    Some(Literal::Integer(v)) => v as i64,
                    _ => panic!("Enum variant value must be an integer."),
                };
                literal_value = Some(LiteralValue::I64(val)); // Assuming I64 for now
                assigned_value = val;
                current_value = val + 1;
            } else {
                literal_value = Some(LiteralValue::I64(current_value));
                assigned_value = current_value;
                current_value += 1;
            }
            variants.push((variant_name, literal_value));

            if !self.check(TokenType::RightBrace) {
                self.expect(vec![TokenType::Comma]).unwrap();
            }
        }

        self.expect(vec![TokenType::RightBrace]).unwrap();
        self.expect(vec![TokenType::SemiColon]).unwrap();

        let enum_type = Type::Enum {
            name: identifier.lexeme.clone().unwrap(),
            variants: variants
                .iter()
                .map(|(name, val_opt)| {
                    let val = match val_opt {
                        Some(LiteralValue::I64(v)) => *v,
                        // Defaulting, though logic above ensures Some(LiteralValue::I64)
                        _ => 0,
                    };
                    (name.lexeme.clone().unwrap(), val)
                })
                .collect(),
            base_type: Box::new(Type::I32), // Defaulting to I32 as per requirement
        };

        // TODO: Add to symbol table correctly
        self.add_symbol(
            identifier.clone(),
            SymbolType::EnumType, // Placeholder
            StorageClass::Global,  // Assuming global for now
            Some(enum_type),
            None,
            None,
            None,
        );

        Node::EnumDecl {
            identifier,
            variants,
        }
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
            self.fn_decl(false).unwrap()
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
                TokenType::Identifier, // For struct/union/enum names
            ])
            .unwrap();

        if ty_token.token_type == TokenType::Identifier {
            let symbol = self.find_symbol(ty_token.clone()).unwrap_or_else(|| {
                panic!(
                    "Unknown type identifier: {} at line {} column {}",
                    ty_token.lexeme.unwrap(),
                    ty_token.line,
                    ty_token.start_column
                )
            });

            let sym_borrow = symbol.borrow();
            match sym_borrow.structure {
                SymbolType::StructType | SymbolType::UnionType | SymbolType::EnumType => {
                    let mut ty = sym_borrow.ty.as_ref().unwrap().clone();
                    // Handle pointers to struct/union/enum
                    for _ in 0..pointers_counter {
                        ty = ty.pointer_to();
                    }
                    // Handle arrays of struct/union/enum
                    if self.match_token(vec![TokenType::LeftBracket]) {
                        let size_token = self.expect(vec![TokenType::Integer]).unwrap();
                        let count = match size_token.value {
                            Some(Literal::Integer(val)) => val,
                            _ => panic!("Expected integer for array size"),
                        };
                        self.expect(vec![TokenType::RightBracket]).unwrap();
                        ty = Type::Array { ty: Box::new(ty), count };
                    }
                    return ty;
                }
                _ => panic!(
                    "Identifier {} is not a type (struct, union, or enum) at line {} column {}",
                    ty_token.lexeme.unwrap(),
                    ty_token.line,
                    ty_token.start_column
                ),
            }
        }

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
                None,
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
                    None,
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
                        operator.line, operator.start_column
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
                    operator.line, operator.start_column
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
                    operator.line, operator.start_column
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
                    start_column: self.previous(1).start_column,
                    end_column: self.previous(1).end_column,
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
                    if operator.token_type != TokenType::Mul {
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
                                start_column: self.previous(1).start_column,
                                end_column: self.previous(1).end_column,
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
                    self.previous(1).start_column,
                    node
                ),
            }

            node = Node::UnaryExpr {
                operator: Token {
                    token_type: TokenType::Mul,
                    lexeme: None,
                    line: self.previous(1).line,
                    start_column: self.previous(1).start_column,
                    end_column: self.previous(1).end_column,
                    value: None,
                },
                right: Box::new(node.clone()),
                ty: node.ty().unwrap().value_at(),
            };
        } else if self.match_token(vec![TokenType::Inc]) {
            node = self.prefix();

            // ensure that the node is an identifier
            match &node {
                Node::LiteralExpr {
                    value: LiteralValue::Identifier(_),
                    ..
                } => {}
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

        node = self.primary();
    }

    // NEW: Loop for postfix operations like '.', '()', '[]'
    // This is a simplified loop focusing on '.' for now.
    // Full integration requires refactoring how (), [], ++, --, and assignments are handled.
    loop {
        if self.match_token(vec![TokenType::Dot]) {
            node = self.parse_field_access(node);
        }
        // TODO: Integrate general '()', '[]', '++', '--' and assignment handling here
        // This will require refactoring the existing postfix() and assignment logic.
        // else if self.match_token(vec![TokenType::LeftParen]) { ... }
        // else if self.match_token(vec![TokenType::LeftBracket]) { ... }
        else {
            break;
        }
    }
    node
}

fn parse_field_access(&mut self, object: Node) -> Node {
    let field_token = self.expect(vec![TokenType::Identifier]).unwrap_or_else(|e| {
        panic!("Expected field name after '.' at line {} column {}: {}", self.previous(1).line, self.previous(1).start_column, e);
    });

    let object_ty = object.ty().unwrap_or_else(|| {
        panic!("Cannot access field on an object with no type at line {} column {}", field_token.line, field_token.start_column);
    });

    match object_ty.clone() { // Clone object_ty to allow moving object into Node::StructFieldAccess
        Type::Struct { name: struct_name, fields, .. } => {
            if let Some((_name, field_type)) = fields.iter().find(|(fname, _)| fname == field_token.lexeme.as_ref().unwrap()) {
                Node::StructFieldAccess {
                    object: Box::new(object),
                    field: field_token,
                    ty: field_type.clone(),
                }
            } else {
                panic!("Struct '{}' has no field named '{}' at line {} column {}", struct_name, field_token.lexeme.as_ref().unwrap(), field_token.line, field_token.start_column);
            }
        }
        Type::Union { name: union_name, fields, .. } => {
             if let Some((_name, field_type)) = fields.iter().find(|(fname, _)| fname == field_token.lexeme.as_ref().unwrap()) {
                Node::StructFieldAccess { // Reusing StructFieldAccess for unions
                    object: Box::new(object),
                    field: field_token,
                    ty: field_type.clone(),
                }
            } else {
                panic!("Union '{}' has no field named '{}' at line {} column {}", union_name, field_token.lexeme.as_ref().unwrap(), field_token.line, field_token.start_column);
            }
        }
        Type::Pointer { ty, .. } => {
            match *ty {
                Type::Struct { name: struct_name, fields, .. } => {
                    if let Some((_name, field_type)) = fields.iter().find(|(fname, _)| fname == field_token.lexeme.as_ref().unwrap()) {
                        // The 'object' is the pointer itself. Codegen needs to handle dereferencing.
                        Node::StructFieldAccess {
                            object: Box::new(object),
                            field: field_token,
                            ty: field_type.clone(),
                        }
                    } else {
                        panic!("Struct '{}' (via pointer) has no field named '{}' at line {} column {}", struct_name, field_token.lexeme.as_ref().unwrap(), field_token.line, field_token.start_column);
                    }
                }
                Type::Union { name: union_name, fields, .. } => {
                    if let Some((_name, field_type)) = fields.iter().find(|(fname, _)| fname == field_token.lexeme.as_ref().unwrap()) {
                        Node::StructFieldAccess {
                            object: Box::new(object),
                            field: field_token,
                            ty: field_type.clone(),
                        }
                    } else {
                        panic!("Union '{}' (via pointer) has no field named '{}' at line {} column {}", union_name, field_token.lexeme.as_ref().unwrap(), field_token.line, field_token.start_column);
                    }
                }
                _ => panic!("Field access through pointer to non-struct/union type {:?} is not allowed with '.' operator at line {} column {}", ty, field_token.line, field_token.start_column),
            }
        }
        _ => panic!("Cannot access field on type {:?} at line {} column {}", object_ty, field_token.line, field_token.start_column),
    }
}


fn postfix(&mut self) -> Node {
    let identifier = self.previous(1);
    match self.find_symbol(identifier.clone()) {
            Some(symbol) => {
                if self.check(TokenType::LeftParen) {
                    if symbol.borrow().structure != SymbolType::Function {
                        panic!(
                            "Expected function at line {} column {}",
                            identifier.line, identifier.start_column
                        );
                    }
                    self.advance();
                    return self.function_call();
                } else if symbol.borrow().structure != SymbolType::Variable {
                    panic!(
                        "Expected variable at line {} column {} got {:?}",
                        identifier.line,
                        identifier.start_column,
                        symbol.borrow().structure
                    );
                }

                let left = if self.match_token(vec![TokenType::LeftBracket]) {
                    self.array_access()
                } else {
                    Node::LiteralExpr {
                        value: LiteralValue::Identifier(symbol.clone()),
                        ty: symbol.borrow().ty.as_ref().unwrap().clone(),
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
            None => {
                eprintln!(
                    "Undefined identifier {} at line {} column {}",
                    identifier.lexeme.clone().unwrap(),
                    identifier.line,
                    identifier.start_column
                );
                std::process::exit(1);
            }
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
        eprintln!(
            "Unexpected token {:?} at line {} column {}",
            token.token_type, token.line, token.start_column
        );
        std::process::exit(1);
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
            self.peek().start_column,
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
        params: Option<Vec<Rc<RefCell<Symbol>>>>,
    ) -> Rc<RefCell<Symbol>> {
            let existing_symbol = self.find_symbol_in_current_scope(identifier.clone());
            if existing_symbol.is_some() {
                let sym_type_str = match existing_symbol.unwrap().borrow().structure {
                    SymbolType::Variable => "Variable",
                    SymbolType::Function => "Function",
                    SymbolType::StructType => "Struct",
                    SymbolType::UnionType => "Union",
                    SymbolType::EnumType => "Enum",
            };
            eprintln!(
                    "{} \"{}\" already declared in this scope at line {} column {}",
                    sym_type_str,
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.start_column
            );
            std::process::exit(1);
        }

        let symbol = Rc::new(RefCell::new(Symbol {
            identifier,
            structure,
            class,
            ty,
            end_label,
            size: None,
            offset,
            params,
        }));

        self.symbols.push(symbol.clone());

        symbol
    }

    fn find_symbol(&self, identifier: Token) -> Option<Rc<RefCell<Symbol>>> {
        let mut symbol: Option<Rc<RefCell<Symbol>>> = None;
        // Search from the end of the symbol table to find the nearest scope
        for it in self.symbols.iter().rev() {
            if it.borrow().identifier.lexeme.clone().unwrap() == identifier.lexeme.clone().unwrap()
            {
                symbol = Some(it.clone());
                // If it's a global or a type definition, it's fine.
                // If it's local/param, it must be in the current function's scope.
                // This logic might need refinement for nested scopes if they are introduced.
                if it.borrow().class == StorageClass::Local || it.borrow().class == StorageClass::Param {
                    // Check if this local/param symbol belongs to the current function
                    // This is a simplification; proper lexical scoping would require more robust handling
                    if self.current_fn.is_some() {
                        let current_fn_name = self.current_fn.as_ref().unwrap().borrow().identifier.lexeme.clone();
                        // This check is imperfect, assumes locals/params are added after function symbol
                        // and cleared on function exit.
                        // A more robust way would be to link symbols to their scope (e.g. function name or ID).
                        let mut found_in_current_fn_scope = false;
                        if let Some(params) = &it.borrow().params { // Check if it's a function symbol itself
                            if it.borrow().identifier.lexeme == current_fn_name {
                                found_in_current_fn_scope = true;
                            }
                        } else if it.borrow().class == StorageClass::Param {
                            // Check if the symbol is a parameter of the current_fn
                            if let Some(current_fn_symbol) = &self.current_fn {
                                if let Some(fn_params) = &current_fn_symbol.borrow().params {
                                    if fn_params.iter().any(|p| p.borrow().identifier.lexeme == it.borrow().identifier.lexeme) {
                                        found_in_current_fn_scope = true;
                                    }
                                }
                            }
                        } else if it.borrow().class == StorageClass::Local {
                             // Assume local is part of current function if current_fn is set.
                             // This needs better scope management for correctness with nested blocks.
                            found_in_current_fn_scope = true;
                        }


                        if found_in_current_fn_scope {
                            break; // Found the most relevant local/param symbol
                        } else {
                            symbol = None; // This local/param is not in the current function's direct scope
                            continue; // Keep searching for a global or another declaration
                        }
                    } else {
                        // No current function, so a local/param symbol here would be out of place
                        // unless it's a global scope variable being shadowed, which find_symbol_in_current_scope should prevent.
                        // For simplicity, we assume global scope if no current_fn.
                         if it.borrow().class == StorageClass::Global {
                            break;
                        } else {
                            symbol = None;
                            continue;
                        }
                    }
                } else { // Global, StructType, UnionType, EnumType
                    break;
                }
            }
        }
        symbol
    }

    // Helper to check for symbols only in the current (innermost) scope.
    // For globals, this is the global scope. For locals, it's within the current function.
    // This is a simplified check. Proper scoping would require managing a stack of symbol tables.
    fn find_symbol_in_current_scope(&self, identifier: Token) -> Option<Rc<RefCell<Symbol>>> {
        for it in self.symbols.iter().rev() {
            if it.borrow().identifier.lexeme.clone().unwrap() == identifier.lexeme.clone().unwrap() {
                // If we are in a function, current scope is locals/params of this function or globals.
                if self.current_fn.is_some() {
                    if it.borrow().class == StorageClass::Local || it.borrow().class == StorageClass::Param {
                        // This is a rough check. Assumes params/locals are associated with current_fn.
                        // A more robust check would involve comparing parent scope IDs or similar.
                        return Some(it.clone());
                    } else if it.borrow().class == StorageClass::Global {
                        // Globals are accessible, but we are checking for re-declarations in *current* scope.
                        // So if 'it' is global, it's not a re-declaration in the *local* current scope.
                        // However, if we are AT global scope (current_fn is None), then finding a global IS a re-declaration.
                        continue; // Keep searching for a local/param with the same name.
                    } else { // StructType, UnionType, EnumType are considered global for now
                        return Some(it.clone());
                    }
                } else {
                    // We are in global scope. Any match is in the current scope.
                    return Some(it.clone());
                }
            }
        }
        None
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
                        operator.line, operator.start_column
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
                            operator.line, operator.start_column
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

    fn fn_decl(&mut self, first_pass: bool) -> Option<Node> {
        self.expect(vec![TokenType::Fn]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        self.expect(vec![TokenType::LeftParen]).unwrap();
        self.reset_offset();
        let params = self.parse_params(first_pass);
        self.expect(vec![TokenType::RightParen]).unwrap();
        let mut ty: Option<Type> = None;

        if self.match_token(vec![TokenType::Colon]) {
            ty = Some(self.parse_type());
        }

        if first_pass {
            let end_label = Some(format!("{}{}", identifier.lexeme.clone().unwrap(), "_end"));

            self.add_symbol(
                identifier.clone(),
                SymbolType::Function,
                StorageClass::Global,
                ty.clone(),
                end_label,
                None,
                Some(params),
            );

            // Skip the body
            self.advance();
            let mut braces = 1;
            while braces != 0 {
                if self.check(TokenType::LeftBrace) {
                    braces += 1;
                } else if self.check(TokenType::RightBrace) {
                    braces -= 1;
                }
                self.advance();
            }

            return None;
        }

        self.current_fn = self.find_symbol(identifier.clone());
        let body = self.compound_statement();
        // delete local variables and parameter symbols
        self.symbols.retain(|x| {
            x.borrow().class != StorageClass::Local && x.borrow().class != StorageClass::Param
        });
        // ensure that the function returns a value if it has a return type in the last statement
        if ty.is_some() {
            match &body {
                Node::CompoundStmt { statements } => {
                    if statements.is_empty() {
                        panic!(
                            "Function {} does not return a value at line {} column {}",
                            identifier.lexeme.clone().unwrap(),
                            identifier.line,
                            identifier.start_column
                        );
                    }

                    let last = statements.last().unwrap();
                    match last {
                        Node::ReturnStmt { .. } => {}
                        _ => panic!(
                            "Function {} does not return a value at line {} column {}",
                            identifier.lexeme.clone().unwrap(),
                            identifier.line,
                            identifier.start_column
                        ),
                    }
                }
                _ => panic!(
                    "Function {} does not return a value at line {} column {}",
                    identifier.lexeme.clone().unwrap(),
                    identifier.line,
                    identifier.start_column
                ),
            }
        }

        self.current_fn = None;

        Some(Node::FnDecl {
            identifier,
            body: Box::new(body),
            stack_size: self.local_offset,
            return_type: ty,
            params,
        })
    }

    fn extern_fn_decl(&mut self, first_pass: bool) {
        self.expect(vec![TokenType::Extern]).unwrap();
        self.expect(vec![TokenType::Fn]).unwrap();
        let identifier = self.expect(vec![TokenType::Identifier]).unwrap();
        self.expect(vec![TokenType::LeftParen]).unwrap();
        self.reset_offset();
        let params = self.parse_params(true);
        self.expect(vec![TokenType::RightParen]).unwrap();
        let mut ty: Option<Type> = None;
        if self.match_token(vec![TokenType::Colon]) {
            ty = Some(self.parse_type());
        }
        if first_pass {
            self.add_symbol(
                identifier,
                SymbolType::Function,
                StorageClass::Global,
                ty,
                None,
                None,
                Some(params),
            );
        }
        self.expect(vec![TokenType::SemiColon]).unwrap();
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
        if op.is_some()
            && (op == Some(TokenType::Add) || op == Some(TokenType::Sub))
            && left_type.is_int()
            && right_type.is_ptr()
        {
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
                identifier.start_column
            );
        }

        let symbol = symbol.unwrap();
        if symbol.borrow().structure != SymbolType::Function {
            panic!(
                "Expected function at line {} column {}",
                identifier.line, identifier.start_column
            );
        }

        let args = self.parse_args();

        self.expect(vec![TokenType::RightParen]).unwrap();

        let ty = symbol.borrow().ty.as_ref().unwrap().clone();
        Node::FnCall {
            identifier,
            args,
            ty,
        }
    }

    fn return_statement(&mut self) -> Node {
        if self.current_fn.is_none() {
            panic!("Return statement outside of function");
        }

        let fn_sym = self.current_fn.clone().unwrap();

        if fn_sym.borrow().ty.is_none() {
            panic!(
                "Function {} has no return type",
                fn_sym.borrow().identifier.lexeme.clone().unwrap()
            );
        }

        let mut expr = self.expression();

        let ty = fn_sym.borrow().ty.as_ref().unwrap().clone();
        expr = match self.modify_type(expr, ty, None) {
            Some(node) => node,
            None => {
                eprintln!(
                    "Incompatible type to return at line {} column {}",
                    self.previous(1).line,
                    self.previous(1).start_column
                );
                std::process::exit(1);
            }
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
                identifier.start_column
            );
        }

        let symbol = symbol.unwrap();
        if symbol.borrow().structure != SymbolType::Variable {
            panic!(
                "Expected variable at line {} column {} got {:?}",
                identifier.line,
                identifier.start_column,
                symbol.borrow().structure
            );
        }

        let symbol = self.find_symbol(identifier.clone()).unwrap_or_else(|| {
            panic!(
                "Variable {} not declared at line {} column {}",
                identifier.lexeme.clone().unwrap(),
                identifier.line,
                identifier.start_column
            )
        });
        let ty = symbol.borrow().ty.as_ref().unwrap().clone();
        let mut left = Node::LiteralExpr {
            value: LiteralValue::Identifier(symbol),
            ty,
        };

        let mut index = self.expression();

        self.expect(vec![TokenType::RightBracket]).unwrap();

        if !index.ty().unwrap().is_int() {
            panic!(
                "Expected integer at line {} column {}",
                self.previous(1).line,
                self.previous(1).start_column
            );
        }

        index = match self.modify_type(index, Type::U32, Some(TokenType::Add)) {
            Some(node) => node,
            None => panic!(
                "Incompatible types at line {} column {}",
                self.previous(1).line,
                self.previous(1).start_column
            ),
        };

        left = Node::BinaryExpr {
            left: Box::new(left.clone()),
            operator: Token {
                token_type: TokenType::Add,
                lexeme: None,
                line: self.previous(1).line,
                start_column: self.previous(1).start_column,
                end_column: self.previous(1).end_column,
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
                start_column: self.previous(1).start_column,
                end_column: self.previous(1).end_column,
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

    fn parse_params(&mut self, first_pass: bool) -> Vec<Rc<RefCell<Symbol>>> {
        let mut params = Vec::new();

        let mut i = 0;
        let mut local_offset = 16;
        while self.check(TokenType::Identifier) {
            let identifier = self.advance();
            self.expect(vec![TokenType::Colon]).unwrap();
            let ty = self.parse_type();
            let offset: isize;
            if i < 6 {
                offset = self.gen_offset(ty.clone());
            } else {
                offset = local_offset;
                local_offset += 8;
            }

            let symbol = if first_pass {
                Rc::new(RefCell::new(Symbol {
                    identifier,
                    structure: SymbolType::Variable,
                    class: StorageClass::Param,
                    ty: Some(ty.clone()),
                    end_label: None,
                    size: None,
                    offset: Some(offset),
                    params: None,
                }))
            } else {
                self.add_symbol(
                    identifier.clone(),
                    SymbolType::Variable,
                    StorageClass::Param,
                    Some(ty.clone()),
                    None,
                    Some(offset),
                    None,
                )
            };

            params.push(symbol);

            if !self.match_token(vec![TokenType::Comma]) {
                break;
            }

            i += 1;
        }

        params
    }

    fn parse_args(&mut self) -> Vec<Node> {
        let mut args = Vec::new();

        while !self.check(TokenType::RightParen) {
            let expr = self.expression();
            args.push(expr);

            if !self.match_token(vec![TokenType::Comma]) {
                break;
            }
        }

        args
    }
}
