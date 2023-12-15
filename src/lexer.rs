use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Integer,
    Float,

    // Keywords.
    Class,
    False,
    Fn,
    For,
    If,
    Null,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    Const,
    While,

    EOF,
}

#[derive(Debug)]
pub enum Literal {
    String(String),
    Integer(u64),
    Float(f64),
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
    pub column: usize,
}

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: String) -> Lexer {
        Lexer {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            keywords: vec![
                ("class".to_string(), TokenType::Class),
                ("false".to_string(), TokenType::False),
                ("fn".to_string(), TokenType::Fn),
                ("for".to_string(), TokenType::For),
                ("if".to_string(), TokenType::If),
                ("null".to_string(), TokenType::Null),
                ("print".to_string(), TokenType::Print),
                ("return".to_string(), TokenType::Return),
                ("super".to_string(), TokenType::Super),
                ("this".to_string(), TokenType::This),
                ("true".to_string(), TokenType::True),
                ("let".to_string(), TokenType::Let),
                ("const".to_string(), TokenType::Const),
                ("while".to_string(), TokenType::While),
            ]
            .into_iter()
            .collect(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, String> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            literal: None,
            line: self.line,
            column: self.column,
        });

        Ok(&self.tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type)
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type)
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type)
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type)
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            '"' => self.string()?,
            _ => {
                if c.is_digit(10) {
                    self.number()?;
                } else if c.is_alphabetic() {
                    self.identifier()?;
                } else {
                    return Err(format!(
                        "Unexpected character '{}' at line {} column {}",
                        c, self.line, self.column
                    ));
                }
            }
        }

        Ok(())
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        self.column += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_with_literal(token_type, None);
    }

    fn add_token_with_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token {
            token_type,
            lexeme: text,
            literal,
            line: self.line,
            column: self.column,
        });
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current).unwrap() != c {
            return false;
        }

        self.current += 1;
        self.column += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.current).unwrap()
    }

    fn string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(format!(
                "Unterminated string at line {} column {}",
                self.line, self.column
            ));
        }

        self.advance();

        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token_with_literal(TokenType::String, Some(Literal::String(value)));

        Ok(())
    }

    fn number(&mut self) -> Result<(), String> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let value = self.source[self.start..self.current].to_string();
        let token_type = if value.contains('.') {
            TokenType::Float
        } else {
            TokenType::Integer
        };

        let literal = match token_type {
            TokenType::Integer => Some(Literal::Integer(value.parse::<u64>().unwrap())),
            TokenType::Float => Some(Literal::Float(value.parse::<f64>().unwrap())),
            _ => None,
        };

        self.add_token_with_literal(token_type, literal);

        Ok(())
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = self.source[self.start..self.current].to_string();
        let token_type = self.keywords.get(&text).unwrap_or(&TokenType::Identifier);

        self.add_token(*token_type);

        Ok(())
    }
}
