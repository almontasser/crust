//! Parser module - Parses tokens into an AST with comprehensive error handling

use crate::ast::{BinOp, Expr, Function, Program, Spanned, SpannedExpr, SpannedStmt, Stmt, UnaryOp};
use crate::errors::{Diagnostic, DiagnosticCollector, DiagnosticEmitter, ErrorCode, Label};
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    diagnostics: DiagnosticCollector,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            diagnostics: DiagnosticCollector::new(),
        }
    }
    
    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.pos.saturating_sub(1)]
    }
    
    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }
    
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }
    
    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }
    
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    
    fn expect(&mut self, expected: TokenKind) -> Result<Token, ()> {
        if self.check(&expected) {
            Ok(self.advance().clone())
        } else {
            let current = self.current().clone();
            self.diagnostics.emit(
                Diagnostic::error(
                    ErrorCode::ExpectedToken,
                    format!("expected {}, found {}", expected.description(), current.kind.description())
                )
                .with_label(Label::primary(current.span, format!("expected {}", expected.description())))
            );
            Err(())
        }
    }
    
    fn expect_identifier(&mut self) -> Result<(String, Span), ()> {
        let token = self.current().clone();
        match &token.kind {
            TokenKind::Ident(name) => {
                let name = name.clone();
                let span = token.span;
                self.advance();
                Ok((name, span))
            }
            _ => {
                self.diagnostics.emit(
                    Diagnostic::error(
                        ErrorCode::ExpectedIdentifier,
                        format!("expected identifier, found {}", token.kind.description())
                    )
                    .with_label(Label::primary(token.span, "expected identifier"))
                );
                Err(())
            }
        }
    }
    
    /// Synchronize after an error to continue parsing
    fn synchronize(&mut self) {
        self.advance();
        
        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }
            
            match self.current().kind {
                TokenKind::Fn | TokenKind::Let | TokenKind::If | 
                TokenKind::While | TokenKind::Return => return,
                _ => {}
            }
            
            self.advance();
        }
    }
    
    pub fn parse(&mut self) -> Result<Program, Vec<Diagnostic>> {
        let mut functions = Vec::new();
        
        while !self.is_at_end() {
            match self.parse_function() {
                Ok(func) => functions.push(func),
                Err(()) => self.synchronize(),
            }
        }
        
        if self.diagnostics.has_errors() {
            Err(self.diagnostics.take_diagnostics())
        } else {
            Ok(Program { functions })
        }
    }
    
    fn parse_function(&mut self) -> Result<Function, ()> {
        let fn_token = self.current().clone();
        self.expect(TokenKind::Fn)?;
        
        let (name, name_span) = self.expect_identifier()?;
        
        let lparen = self.expect(TokenKind::LParen)?;
        
        let mut params = Vec::new();
        if !self.check(&TokenKind::RParen) {
            loop {
                let (param_name, param_span) = self.expect_identifier()?;
                params.push((param_name, param_span));
                
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        // Check for matching closing paren
        if self.current().kind != TokenKind::RParen {
            let current = self.current().clone();
            self.diagnostics.emit(
                Diagnostic::error(ErrorCode::UnmatchedParen, "unclosed `(`")
                    .with_label(Label::primary(current.span, format!("expected `)`, found {}", current.kind.description())))
                    .with_label(Label::secondary(lparen.span, "opening `(` here"))
            );
            return Err(());
        }
        self.advance(); // consume )
        
        let lbrace = self.expect(TokenKind::LBrace)?;
        
        let mut body = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => body.push(stmt),
                Err(()) => self.synchronize(),
            }
        }
        
        // Check for matching closing brace
        if self.current().kind != TokenKind::RBrace {
            let current = self.current().clone();
            self.diagnostics.emit(
                Diagnostic::error(ErrorCode::UnmatchedBrace, "unclosed `{`")
                    .with_label(Label::primary(current.span, "expected `}`"))
                    .with_label(Label::secondary(lbrace.span, "opening `{` here"))
                    .with_note("you may be missing a `}` to close this block")
            );
            return Err(());
        }
        let rbrace = self.advance();
        
        let span = Span::new(fn_token.span.start, rbrace.span.end);
        
        Ok(Function {
            name,
            name_span,
            params,
            body,
            span,
        })
    }
    
    fn parse_statement(&mut self) -> Result<SpannedStmt, ()> {
        let start_span = self.current().span;
        
        let stmt = if self.match_token(&TokenKind::Let) {
            let (name, name_span) = self.expect_identifier()?;
            self.expect(TokenKind::Eq)?;
            let value = self.parse_expr()?;
            Stmt::Let { name, name_span, value }
        } else if self.match_token(&TokenKind::Return) {
            if self.check(&TokenKind::Semicolon) {
                Stmt::Return(None)
            } else {
                Stmt::Return(Some(self.parse_expr()?))
            }
        } else {
            Stmt::Expr(self.parse_expr()?)
        };
        
        let semicolon = self.expect(TokenKind::Semicolon)?;
        let span = Span::new(start_span.start, semicolon.span.end);
        
        Ok(Spanned::new(stmt, span))
    }
    
    fn parse_expr(&mut self) -> Result<SpannedExpr, ()> {
        self.parse_or()
    }
    
    fn parse_or(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_and()?;
        
        while self.match_token(&TokenKind::Or) {
            let right = self.parse_and()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op: BinOp::Or,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_and(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_equality()?;
        
        while self.match_token(&TokenKind::And) {
            let right = self.parse_equality()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op: BinOp::And,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_equality(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_comparison()?;
        
        loop {
            let op = if self.match_token(&TokenKind::EqEq) {
                BinOp::Eq
            } else if self.match_token(&TokenKind::Ne) {
                BinOp::Ne
            } else {
                break;
            };
            
            let right = self.parse_comparison()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_comparison(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_additive()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Lt) {
                BinOp::Lt
            } else if self.match_token(&TokenKind::Le) {
                BinOp::Le
            } else if self.match_token(&TokenKind::Gt) {
                BinOp::Gt
            } else if self.match_token(&TokenKind::Ge) {
                BinOp::Ge
            } else {
                break;
            };
            
            let right = self.parse_additive()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_additive(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_multiplicative()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Plus) {
                BinOp::Add
            } else if self.match_token(&TokenKind::Minus) {
                BinOp::Sub
            } else {
                break;
            };
            
            let right = self.parse_multiplicative()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_multiplicative(&mut self) -> Result<SpannedExpr, ()> {
        let mut left = self.parse_unary()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Star) {
                BinOp::Mul
            } else if self.match_token(&TokenKind::Slash) {
                BinOp::Div
            } else if self.match_token(&TokenKind::Percent) {
                BinOp::Mod
            } else {
                break;
            };
            
            let right = self.parse_unary()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Spanned::new(
                Expr::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );
        }
        
        Ok(left)
    }
    
    fn parse_unary(&mut self) -> Result<SpannedExpr, ()> {
        let token = self.current().clone();
        
        if self.match_token(&TokenKind::Minus) {
            let operand = self.parse_unary()?;
            let span = Span::new(token.span.start, operand.span.end);
            return Ok(Spanned::new(
                Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                span,
            ));
        }
        
        if self.match_token(&TokenKind::Not) {
            let operand = self.parse_unary()?;
            let span = Span::new(token.span.start, operand.span.end);
            return Ok(Spanned::new(
                Expr::UnaryOp {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                span,
            ));
        }
        
        self.parse_call()
    }
    
    fn parse_call(&mut self) -> Result<SpannedExpr, ()> {
        let expr = self.parse_primary()?;
        
        // Check if it's a function call
        if let Expr::Identifier(name) = &expr.node {
            if self.check(&TokenKind::LParen) {
                let name = name.clone();
                let name_span = expr.span;
                let lparen = self.advance().clone();
                
                let mut args = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    args.push(self.parse_expr()?);
                    
                    while self.match_token(&TokenKind::Comma) {
                        args.push(self.parse_expr()?);
                    }
                }
                
                // Check for closing paren
                if self.current().kind != TokenKind::RParen {
                    let current = self.current().clone();
                    self.diagnostics.emit(
                        Diagnostic::error(ErrorCode::UnmatchedParen, "unclosed `(` in function call")
                            .with_label(Label::primary(current.span, format!("expected `)`, found {}", current.kind.description())))
                            .with_label(Label::secondary(lparen.span, "opening `(` here"))
                            .with_help("add `)` to close the function call arguments")
                    );
                    return Err(());
                }
                let rparen = self.advance();
                
                let span = Span::new(name_span.start, rparen.span.end);
                return Ok(Spanned::new(
                    Expr::Call { name, name_span, args },
                    span,
                ));
            }
        }
        
        Ok(expr)
    }
    
    fn parse_primary(&mut self) -> Result<SpannedExpr, ()> {
        let token = self.current().clone();
        
        match &token.kind {
            TokenKind::Integer(n) => {
                let n = *n;
                self.advance();
                Ok(Spanned::new(Expr::Integer(n), token.span))
            }
            TokenKind::True => {
                self.advance();
                Ok(Spanned::new(Expr::Boolean(true), token.span))
            }
            TokenKind::False => {
                self.advance();
                Ok(Spanned::new(Expr::Boolean(false), token.span))
            }
            TokenKind::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Spanned::new(Expr::String(s), token.span))
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(Spanned::new(Expr::Identifier(name), token.span))
            }
            TokenKind::LParen => {
                let lparen = token.clone();
                self.advance();
                
                let expr = self.parse_expr()?;
                
                if self.current().kind != TokenKind::RParen {
                    let current = self.current().clone();
                    self.diagnostics.emit(
                        Diagnostic::error(ErrorCode::UnmatchedParen, "unclosed `(`")
                            .with_label(Label::primary(current.span, format!("expected `)`, found {}", current.kind.description())))
                            .with_label(Label::secondary(lparen.span, "opening `(` here"))
                    );
                    return Err(());
                }
                let rparen = self.advance();
                
                let span = Span::new(lparen.span.start, rparen.span.end);
                Ok(Spanned::new(Expr::Grouped(Box::new(expr)), span))
            }
            TokenKind::Eof => {
                self.diagnostics.emit(
                    Diagnostic::error(ErrorCode::ExpectedExpression, "unexpected end of file")
                        .with_label(Label::primary(token.span, "expected expression"))
                );
                Err(())
            }
            _ => {
                self.diagnostics.emit(
                    Diagnostic::error(
                        ErrorCode::ExpectedExpression,
                        format!("expected expression, found {}", token.kind.description())
                    )
                    .with_label(Label::primary(token.span, "expected expression"))
                    .with_note("expressions can be literals, identifiers, or operators")
                );
                Err(())
            }
        }
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
    
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.take_diagnostics()
    }
}
