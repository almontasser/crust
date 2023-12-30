use std::collections::HashMap;

use crate::utils::RNG;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(u64),
    Identifier(String),
    String { value: String, label: String },
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
    String,

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
    I8,
    I16,
    I32,
    I64,
    While,
    Char,

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
    LeftBracket,
    RightBracket,
    Or,
    Xor,
    LogicalNot,
    Invert,

    // Double-character tokens
    Equal,
    NotEqual,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
    Inc,
    Dec,

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
    string_labels: Vec<String>,
    rng: RNG,
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
                keywords.insert(String::from("i8"), TokenType::I8);
                keywords.insert(String::from("i16"), TokenType::I16);
                keywords.insert(String::from("i32"), TokenType::I32);
                keywords.insert(String::from("i64"), TokenType::I64);
                keywords.insert(String::from("while"), TokenType::While);
                keywords.insert(String::from("char"), TokenType::Char);
                keywords
            },
            string_labels: Vec::new(),
            rng: RNG::new(),
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
            '+' => {
                if self.match_char('+') {
                    self.add_token(TokenType::Inc);
                } else {
                    self.add_token(TokenType::Add);
                }
            }
            '-' => {
                if self.match_char('-') {
                    self.add_token(TokenType::Dec);
                } else {
                    self.add_token(TokenType::Sub);
                }
            }
            '*' => self.add_token(TokenType::Mul),
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Div);
                }
            }
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ';' => self.add_token(TokenType::SemiColon),
            ':' => self.add_token(TokenType::Colon),
            ',' => self.add_token(TokenType::Comma),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
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
                    self.add_token(TokenType::LogicalNot);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessThanOrEqual);
                } else if self.match_char('<') {
                    self.add_token(TokenType::LeftShift);
                } else {
                    self.add_token(TokenType::LessThan);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterThanOrEqual);
                } else if self.match_char('>') {
                    self.add_token(TokenType::RightShift);
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
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::LogicalOr);
                } else {
                    self.add_token(TokenType::Or);
                }
            }
            '^' => self.add_token(TokenType::Xor),
            '~' => self.add_token(TokenType::Invert),
            ' ' | '\t' | '\r' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            '\'' => self.character(),
            '"' => self.string(),
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

    fn escape_char(&self, c: char) -> char {
        match c {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            '\'' => '\'',
            '\\' => '\\',
            '"' => '"',
            _ => panic!("Unexpected escape character: {}", c),
        }
    }

    fn character(&mut self) {
        let mut c = self.advance();
        if c == '\\' {
            c = self.advance();
            c = self.escape_char(c);
        }

        if self.advance() != '\'' {
            panic!("Expected closing quote");
        }

        self.add_token_literal(TokenType::Integer, Some(Literal::Integer(c as u64)));
    }

    fn string(&mut self) {
        let mut str = String::new();
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            } else if self.peek() == '\\' {
                self.advance();
                let c = self.advance();
                str.push(self.escape_char(c));
            } else {
                str.push(self.advance());
            }
        }

        if self.is_at_end() {
            panic!("Unterminated string");
        }

        self.advance();

        let label = self.label_for_str(str.clone());
        self.add_token_literal(
            TokenType::String,
            Some(Literal::String {
                value: str,
                label: label.clone(),
            }),
        );
    }

    fn label_for_str(&mut self, s: String) -> String {
        let mut label = String::from("str_");
        for (i, c) in s.char_indices() {
            if i > 4 {
                if self.string_labels.contains(&label) {
                    // add random hex digit to label
                    label.push_str(&format!("{:x}", self.rng.random() % 16));
                    continue;
                } else {
                    break;
                }
            }
            label.push_str(&format!("{:x}", c as u32));
        }
        self.string_labels.push(label.clone());
        label
    }
}
