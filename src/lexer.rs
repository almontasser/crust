#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Integer(u64),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
    pub column: usize,
    pub value: Option<Literal>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Add,
    Sub,
    Mul,
    Div,

    Integer,

    EOF,
}

pub struct Lexer {
    tokens: Vec<Token>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
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
            ' ' | '\t' | '\r' => {}
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            c if c.is_digit(10) => self.number(),
            _ => panic!("Unexpected character: {}", c),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
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
}
