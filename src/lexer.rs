//! Lexer module - Tokenizes source code into tokens with span information

use crate::errors::{Diagnostic, DiagnosticCollector, DiagnosticEmitter, ErrorCode, Label};
use crate::span::{Position, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Fn,
    Let,
    If,
    Else,
    While,
    Return,
    True,
    False,
    
    // Identifiers and literals
    Ident(String),
    Integer(i64),
    String(String),
    
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Not,
    
    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Colon,
    Arrow,
    
    // Special
    Eof,
    Error, // Represents a lexer error token
}

impl TokenKind {
    pub fn description(&self) -> &'static str {
        match self {
            TokenKind::Fn => "`fn`",
            TokenKind::Let => "`let`",
            TokenKind::If => "`if`",
            TokenKind::Else => "`else`",
            TokenKind::While => "`while`",
            TokenKind::Return => "`return`",
            TokenKind::True => "`true`",
            TokenKind::False => "`false`",
            TokenKind::Ident(_) => "identifier",
            TokenKind::Integer(_) => "integer literal",
            TokenKind::String(_) => "string literal",
            TokenKind::Plus => "`+`",
            TokenKind::Minus => "`-`",
            TokenKind::Star => "`*`",
            TokenKind::Slash => "`/`",
            TokenKind::Percent => "`%`",
            TokenKind::Eq => "`=`",
            TokenKind::EqEq => "`==`",
            TokenKind::Ne => "`!=`",
            TokenKind::Lt => "`<`",
            TokenKind::Le => "`<=`",
            TokenKind::Gt => "`>`",
            TokenKind::Ge => "`>=`",
            TokenKind::And => "`&&`",
            TokenKind::Or => "`||`",
            TokenKind::Not => "`!`",
            TokenKind::LParen => "`(`",
            TokenKind::RParen => "`)`",
            TokenKind::LBrace => "`{`",
            TokenKind::RBrace => "`}`",
            TokenKind::LBracket => "`[`",
            TokenKind::RBracket => "`]`",
            TokenKind::Semicolon => "`;`",
            TokenKind::Comma => "`,`",
            TokenKind::Colon => "`:`",
            TokenKind::Arrow => "`->`",
            TokenKind::Eof => "end of file",
            TokenKind::Error => "error",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, lexeme: String) -> Self {
        Token { kind, span, lexeme }
    }
    
    pub fn eof(pos: Position) -> Self {
        Token {
            kind: TokenKind::Eof,
            span: Span::new(pos, pos),
            lexeme: String::new(),
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
    offset: usize,
    diagnostics: DiagnosticCollector,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
            offset: 0,
            diagnostics: DiagnosticCollector::new(),
        }
    }
    
    fn current_position(&self) -> Position {
        Position::new(self.line, self.column, self.offset)
    }
    
    fn current(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }
    
    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos + 1).copied()
    }
    
    fn advance(&mut self) -> Option<char> {
        let ch = self.current();
        if let Some(c) = ch {
            self.pos += 1;
            self.offset += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        ch
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == '/' && self.peek() == Some('/') {
                // Single-line comment
                self.advance(); // consume first /
                self.advance(); // consume second /
                while let Some(c) = self.current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }
    
    fn read_identifier(&mut self, start_pos: Position) -> Token {
        let start_offset = self.pos;
        while let Some(ch) = self.current() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        let lexeme: String = self.chars[start_offset..self.pos].iter().collect();
        let end_pos = self.current_position();
        let span = Span::new(start_pos, end_pos);
        
        let kind = match lexeme.as_str() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(lexeme.clone()),
        };
        
        Token::new(kind, span, lexeme)
    }
    
    fn read_number(&mut self, start_pos: Position) -> Token {
        let start_offset = self.pos;
        while let Some(ch) = self.current() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch.is_alphabetic() || ch == '_' {
                // Invalid character in number
                let error_start = self.current_position();
                while let Some(c) = self.current() {
                    if c.is_alphanumeric() || c == '_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let error_end = self.current_position();
                let lexeme: String = self.chars[start_offset..self.pos].iter().collect();
                let span = Span::new(start_pos, error_end);
                
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::InvalidNumber, format!("invalid digit in integer literal"))
                        .with_label(Label::primary(
                            Span::new(error_start, error_end),
                            "invalid digit",
                        ))
                        .with_note("integer literals can only contain digits 0-9")
                );
                
                return Token::new(TokenKind::Error, span, lexeme);
            } else {
                break;
            }
        }
        
        let lexeme: String = self.chars[start_offset..self.pos].iter().collect();
        let end_pos = self.current_position();
        let span = Span::new(start_pos, end_pos);
        
        match lexeme.parse::<i64>() {
            Ok(n) => Token::new(TokenKind::Integer(n), span, lexeme),
            Err(_) => {
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::InvalidNumber, "integer literal is too large")
                        .with_label(Label::primary(span, "value exceeds maximum integer size"))
                        .with_help("the maximum value for an integer is 9223372036854775807")
                );
                Token::new(TokenKind::Error, span, lexeme)
            }
        }
    }
    
    fn read_string(&mut self, start_pos: Position) -> Token {
        self.advance(); // consume opening quote
        let content_start = self.pos;
        let mut string_content = String::new();
        let mut has_error = false;
        
        loop {
            match self.current() {
                None | Some('\n') => {
                    let end_pos = self.current_position();
                    let span = Span::new(start_pos, end_pos);
                    let lexeme: String = self.chars[content_start..self.pos].iter().collect();
                    
                    self.diagnostics.emit(
                        Diagnostic::error(ErrorCode::UnterminatedString, "unterminated string literal")
                            .with_label(Label::primary(span, "string literal starts here"))
                            .with_help("add a closing `\"` to terminate the string")
                    );
                    
                    return Token::new(TokenKind::Error, span, lexeme);
                }
                Some('"') => {
                    self.advance(); // consume closing quote
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.current() {
                        Some('n') => { string_content.push('\n'); self.advance(); }
                        Some('t') => { string_content.push('\t'); self.advance(); }
                        Some('r') => { string_content.push('\r'); self.advance(); }
                        Some('\\') => { string_content.push('\\'); self.advance(); }
                        Some('"') => { string_content.push('"'); self.advance(); }
                        Some(c) => {
                            let escape_pos = self.current_position();
                            self.advance();
                            let escape_end = self.current_position();
                            
                            self.diagnostics.emit(
                                Diagnostic::error(ErrorCode::UnexpectedCharacter, 
                                    format!("unknown escape sequence: \\{}", c))
                                    .with_label(Label::primary(
                                        Span::new(escape_pos, escape_end),
                                        "unknown escape sequence"
                                    ))
                                    .with_help("valid escape sequences are: \\n, \\t, \\r, \\\\, \\\"")
                            );
                            has_error = true;
                        }
                        None => {
                            // Will be caught by the unterminated string check
                        }
                    }
                }
                Some(c) => {
                    string_content.push(c);
                    self.advance();
                }
            }
        }
        
        let end_pos = self.current_position();
        let span = Span::new(start_pos, end_pos);
        let lexeme: String = self.chars[content_start..self.pos.saturating_sub(1)].iter().collect();
        
        if has_error {
            Token::new(TokenKind::Error, span, lexeme)
        } else {
            Token::new(TokenKind::String(string_content), span, lexeme)
        }
    }
    
    fn make_token(&self, kind: TokenKind, start_pos: Position, lexeme: &str) -> Token {
        Token::new(kind, Span::new(start_pos, self.current_position()), lexeme.to_string())
    }
    
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        
        let start_pos = self.current_position();
        
        let ch = match self.current() {
            None => return Token::eof(start_pos),
            Some(ch) => ch,
        };
        
        // Single character tokens
        match ch {
            '+' => { self.advance(); return self.make_token(TokenKind::Plus, start_pos, "+"); }
            '*' => { self.advance(); return self.make_token(TokenKind::Star, start_pos, "*"); }
            '/' => { self.advance(); return self.make_token(TokenKind::Slash, start_pos, "/"); }
            '%' => { self.advance(); return self.make_token(TokenKind::Percent, start_pos, "%"); }
            '(' => { self.advance(); return self.make_token(TokenKind::LParen, start_pos, "("); }
            ')' => { self.advance(); return self.make_token(TokenKind::RParen, start_pos, ")"); }
            '{' => { self.advance(); return self.make_token(TokenKind::LBrace, start_pos, "{"); }
            '}' => { self.advance(); return self.make_token(TokenKind::RBrace, start_pos, "}"); }
            '[' => { self.advance(); return self.make_token(TokenKind::LBracket, start_pos, "["); }
            ']' => { self.advance(); return self.make_token(TokenKind::RBracket, start_pos, "]"); }
            ';' => { self.advance(); return self.make_token(TokenKind::Semicolon, start_pos, ";"); }
            ',' => { self.advance(); return self.make_token(TokenKind::Comma, start_pos, ","); }
            ':' => { self.advance(); return self.make_token(TokenKind::Colon, start_pos, ":"); }
            _ => {}
        }
        
        // Multi-character tokens
        match ch {
            '-' => {
                self.advance();
                if self.current() == Some('>') {
                    self.advance();
                    return self.make_token(TokenKind::Arrow, start_pos, "->");
                }
                return self.make_token(TokenKind::Minus, start_pos, "-");
            }
            '=' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    return self.make_token(TokenKind::EqEq, start_pos, "==");
                }
                return self.make_token(TokenKind::Eq, start_pos, "=");
            }
            '!' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    return self.make_token(TokenKind::Ne, start_pos, "!=");
                }
                return self.make_token(TokenKind::Not, start_pos, "!");
            }
            '<' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    return self.make_token(TokenKind::Le, start_pos, "<=");
                }
                return self.make_token(TokenKind::Lt, start_pos, "<");
            }
            '>' => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    return self.make_token(TokenKind::Ge, start_pos, ">=");
                }
                return self.make_token(TokenKind::Gt, start_pos, ">");
            }
            '&' => {
                self.advance();
                if self.current() == Some('&') {
                    self.advance();
                    return self.make_token(TokenKind::And, start_pos, "&&");
                }
                // Single & not supported yet
                let end_pos = self.current_position();
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::UnexpectedCharacter, "unexpected character `&`")
                        .with_label(Label::primary(Span::new(start_pos, end_pos), "unexpected character"))
                        .with_help("did you mean `&&` for logical AND?")
                );
                return Token::new(TokenKind::Error, Span::new(start_pos, end_pos), "&".to_string());
            }
            '|' => {
                self.advance();
                if self.current() == Some('|') {
                    self.advance();
                    return self.make_token(TokenKind::Or, start_pos, "||");
                }
                // Single | not supported yet
                let end_pos = self.current_position();
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::UnexpectedCharacter, "unexpected character `|`")
                        .with_label(Label::primary(Span::new(start_pos, end_pos), "unexpected character"))
                        .with_help("did you mean `||` for logical OR?")
                );
                return Token::new(TokenKind::Error, Span::new(start_pos, end_pos), "|".to_string());
            }
            '"' => {
                return self.read_string(start_pos);
            }
            _ if ch.is_ascii_digit() => {
                return self.read_number(start_pos);
            }
            _ if ch.is_alphabetic() || ch == '_' => {
                return self.read_identifier(start_pos);
            }
            _ => {
                self.advance();
                let end_pos = self.current_position();
                let span = Span::new(start_pos, end_pos);
                
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::UnexpectedCharacter, 
                        format!("unexpected character: `{}`", ch))
                        .with_label(Label::primary(span, "unexpected character"))
                        .with_note(format!("character code: U+{:04X}", ch as u32))
                );
                
                return Token::new(TokenKind::Error, span, ch.to_string());
            }
        }
    }
    
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
    
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.take_diagnostics()
    }
}
