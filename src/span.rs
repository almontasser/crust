//! Span module - Source location tracking

/// Represents a position in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Position { line, column, offset }
    }
}

/// Represents a span in the source code (start to end position)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }
    
    pub fn from_positions(start_line: usize, start_col: usize, start_offset: usize,
                          end_line: usize, end_col: usize, end_offset: usize) -> Self {
        Span {
            start: Position::new(start_line, start_col, start_offset),
            end: Position::new(end_line, end_col, end_offset),
        }
    }
    
    /// Merge two spans into one that covers both
    pub fn merge(&self, other: &Span) -> Span {
        let start = if self.start.offset <= other.start.offset {
            self.start
        } else {
            other.start
        };
        let end = if self.end.offset >= other.end.offset {
            self.end
        } else {
            other.end
        };
        Span { start, end }
    }
}

/// A value with an associated span
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}
