use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(u64),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
    pub column: usize,
    pub value: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Integer,

    // Keywords
    Else,
    Fn,
    For,
    If,
    Let,
    Return,
    U8,
    U16,
    U32,
    U64,
    While,

    // Single-character tokens
    Add,
    Sub,
    Mul,
    Div,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    SemiColon,
    Assign,
    LessThan,
    GreaterThan,
    Colon,
    Ampersand,
    Comma,

    // Double-character tokens
    Equal,
    NotEqual,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,

    EOF,
}

pub struct Lexer {
    tokens: Vec<Token>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            tokens: Vec::new(),
            source,
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            keywords: {
                let mut keywords = HashMap::new();
                keywords.insert(String::from("else"), TokenType::Else);
                keywords.insert(String::from("fn"), TokenType::Fn);
                keywords.insert(String::from("for"), TokenType::For);
                keywords.insert(String::from("if"), TokenType::If);
                keywords.insert(String::from("let"), TokenType::Let);
                keywords.insert(String::from("return"), TokenType::Return);
                keywords.insert(String::from("u8"), TokenType::U8);
                keywords.insert(String::from("u16"), TokenType::U16);
                keywords.insert(String::from("u32"), TokenType::U32);
                keywords.insert(String::from("u64"), TokenType::U64);
                keywords.insert(String::from("while"), TokenType::While);
                keywords
            },
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: None,
            line: self.line,
            column: self.column,
            value: None,
        });

        &self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '+' => self.add_token(TokenType::Add),
            '-' => self.add_token(TokenType::Sub),
            '*' => self.add_token(TokenType::Mul),
            '/' => self.add_token(TokenType::Div),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ';' => self.add_token(TokenType::SemiColon),
            ':' => self.add_token(TokenType::Colon),
            ',' => self.add_token(TokenType::Comma),
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::Equal);
                } else {
                    self.add_token(TokenType::Assign);
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::NotEqual);
                } else {
                    panic!("Unexpected character: {}", c);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessThanOrEqual);
                } else {
                    self.add_token(TokenType::LessThan);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterThanOrEqual);
                } else {
                    self.add_token(TokenType::GreaterThan);
                }
            }
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::LogicalAnd);
                } else {
                    self.add_token(TokenType::Ampersand);
                }
            }
            ' ' | '\t' | '\r' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            c if c.is_digit(10) => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        self.column += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, None);
    }

    fn add_token_literal(&mut self, token_type: TokenType, none: Option<Literal>) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token {
            token_type: token_type,
            lexeme: Some(text.to_string()),
            line: self.line,
            column: self.column,
            value: none,
        });
    }

    fn number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let value = text.parse::<u64>().unwrap();

        self.add_token_literal(TokenType::Integer, Some(Literal::Integer(value)));
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let token_type = self.keywords.get(text).unwrap_or(&TokenType::Identifier);

        if *token_type == TokenType::Identifier {
            self.add_token_literal(*token_type, Some(Literal::Identifier(text.to_string())));
        } else {
            self.add_token(*token_type);
        }
    }

    fn match_char(&mut self, arg: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != arg {
            return false;
        }

        self.current += 1;
        self.column += 1;
        true
    }
}
