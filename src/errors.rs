//! Error handling module - Comprehensive error reporting with beautiful CLI output

use crate::span::Span;

/// Error severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
    pub fn color(&self) -> &'static str {
        match self {
            Severity::Error => "\x1b[1;31m",   // Bold red
            Severity::Warning => "\x1b[1;33m", // Bold yellow
            Severity::Note => "\x1b[1;36m",    // Bold cyan
        }
    }
    
    pub fn label(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    }
}

/// Error codes for different types of errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    // Lexer errors (E0001 - E0099)
    UnexpectedCharacter,
    UnterminatedString,
    InvalidNumber,
    
    // Parser errors (E0100 - E0199)
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedToken,
    UnmatchedParen,
    UnmatchedBrace,
    
    // Semantic errors (E0200 - E0299)
    UndefinedFunction,
    WrongArgumentCount,
    UndefinedVariable,
    
    // Code generation errors (E0300 - E0399)
    CodeGenError,
}

impl ErrorCode {
    pub fn code(&self) -> &'static str {
        match self {
            ErrorCode::UnexpectedCharacter => "E0001",
            ErrorCode::UnterminatedString => "E0002",
            ErrorCode::InvalidNumber => "E0003",
            ErrorCode::UnexpectedToken => "E0100",
            ErrorCode::ExpectedExpression => "E0101",
            ErrorCode::ExpectedIdentifier => "E0102",
            ErrorCode::ExpectedToken => "E0103",
            ErrorCode::UnmatchedParen => "E0104",
            ErrorCode::UnmatchedBrace => "E0105",
            ErrorCode::UndefinedFunction => "E0200",
            ErrorCode::WrongArgumentCount => "E0201",
            ErrorCode::UndefinedVariable => "E0202",
            ErrorCode::CodeGenError => "E0300",
        }
    }
}

/// A diagnostic label pointing to a specific span
#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
    pub is_primary: bool,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        Label {
            span,
            message: message.into(),
            is_primary: true,
        }
    }
    
    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        Label {
            span,
            message: message.into(),
            is_primary: false,
        }
    }
}

/// A complete diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: ErrorCode,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

impl Diagnostic {
    pub fn error(code: ErrorCode, message: impl Into<String>) -> Self {
        Diagnostic {
            severity: Severity::Error,
            code,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn warning(code: ErrorCode, message: impl Into<String>) -> Self {
        Diagnostic {
            severity: Severity::Warning,
            code,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }
    
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
    
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

/// Error reporter that formats and displays diagnostics
pub struct ErrorReporter<'a> {
    source: &'a str,
    filename: &'a str,
    lines: Vec<&'a str>,
}

impl<'a> ErrorReporter<'a> {
    pub fn new(source: &'a str, filename: &'a str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        ErrorReporter { source, filename, lines }
    }
    
    /// Get the line number width for formatting
    fn line_number_width(&self, diagnostics: &[Diagnostic]) -> usize {
        let max_line = diagnostics
            .iter()
            .flat_map(|d| d.labels.iter())
            .map(|l| l.span.end.line)
            .max()
            .unwrap_or(1);
        max_line.to_string().len()
    }
    
    /// Format a single diagnostic
    pub fn format_diagnostic(&self, diagnostic: &Diagnostic) -> String {
        let mut output = String::new();
        let reset = "\x1b[0m";
        let bold = "\x1b[1m";
        let blue = "\x1b[1;34m";
        let cyan = "\x1b[1;36m";
        
        // Header: error[E0001]: message
        output.push_str(&format!(
            "{}{}[{}]{}: {}{}\n",
            diagnostic.severity.color(),
            diagnostic.severity.label(),
            diagnostic.code.code(),
            reset,
            bold,
            diagnostic.message
        ));
        output.push_str(reset);
        
        if diagnostic.labels.is_empty() {
            return output;
        }
        
        // Get the primary label for location info
        let primary_label = diagnostic.labels.iter().find(|l| l.is_primary);
        
        if let Some(label) = primary_label {
            let line_num_width = label.span.end.line.to_string().len().max(2);
            let padding = " ".repeat(line_num_width);
            
            // File location: --> file.cr:1:5
            output.push_str(&format!(
                "  {}-->{} {}:{}:{}\n",
                blue, reset,
                self.filename,
                label.span.start.line,
                label.span.start.column
            ));
            
            // Empty line with bar
            output.push_str(&format!("  {} {}|{}\n", padding, blue, reset));
            
            // Group labels by line
            let mut labels_by_line: std::collections::BTreeMap<usize, Vec<&Label>> = 
                std::collections::BTreeMap::new();
            for label in &diagnostic.labels {
                labels_by_line
                    .entry(label.span.start.line)
                    .or_default()
                    .push(label);
            }
            
            // Print each line with its labels
            for (line_num, labels) in &labels_by_line {
                if *line_num > 0 && *line_num <= self.lines.len() {
                    let line_content = self.lines[*line_num - 1];
                    
                    // Print the source line
                    output.push_str(&format!(
                        "  {}{:>width$} |{} {}\n",
                        blue, line_num, reset, line_content,
                        width = line_num_width
                    ));
                    
                    // Print underlines for each label on this line
                    for label in labels {
                        let start_col = label.span.start.column;
                        let end_col = if label.span.start.line == label.span.end.line {
                            label.span.end.column
                        } else {
                            line_content.len() + 1
                        };
                        
                        let underline_char = if label.is_primary { '^' } else { '-' };
                        let color = if label.is_primary {
                            diagnostic.severity.color()
                        } else {
                            cyan
                        };
                        
                        let spaces = " ".repeat(start_col.saturating_sub(1));
                        let underline = underline_char.to_string()
                            .repeat((end_col - start_col).max(1));
                        
                        output.push_str(&format!(
                            "  {} {}|{} {}{}{} {}{}\n",
                            padding, blue, reset,
                            spaces, color, underline,
                            label.message, reset
                        ));
                    }
                }
            }
            
            // Empty line with bar
            output.push_str(&format!("  {} {}|{}\n", padding, blue, reset));
        }
        
        // Print notes
        for note in &diagnostic.notes {
            output.push_str(&format!(
                "  {}= note{}: {}\n",
                cyan, reset, note
            ));
        }
        
        // Print help
        if let Some(help) = &diagnostic.help {
            output.push_str(&format!(
                "  {}= help{}: {}\n",
                cyan, reset, help
            ));
        }
        
        output
    }
    
    /// Report all diagnostics and return the number of errors
    pub fn report(&self, diagnostics: &[Diagnostic]) -> usize {
        let mut error_count = 0;
        
        for diagnostic in diagnostics {
            eprintln!("{}", self.format_diagnostic(diagnostic));
            if diagnostic.severity == Severity::Error {
                error_count += 1;
            }
        }
        
        if error_count > 0 {
            let reset = "\x1b[0m";
            let bold_red = "\x1b[1;31m";
            eprintln!(
                "{}error{}: aborting due to {} previous error{}",
                bold_red, reset,
                error_count,
                if error_count == 1 { "" } else { "s" }
            );
        }
        
        error_count
    }
}

/// Result type for compilation operations
pub type CompileResult<T> = Result<T, Vec<Diagnostic>>;

/// Trait for types that can accumulate and report errors
pub trait DiagnosticEmitter {
    fn emit(&mut self, diagnostic: Diagnostic);
    fn has_errors(&self) -> bool;
    fn take_diagnostics(&mut self) -> Vec<Diagnostic>;
}

/// Default implementation of a diagnostic collector
#[derive(Debug, Default)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollector {
    pub fn new() -> Self {
        DiagnosticCollector {
            diagnostics: Vec::new(),
        }
    }
}

impl DiagnosticEmitter for DiagnosticCollector {
    fn emit(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
    
    fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }
    
    fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}
