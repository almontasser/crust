//! Types module - Defines the type system for Crust

use std::fmt;

/// Represents a type in the Crust language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Signed integers
    /// 8-bit signed integer
    I8,
    /// 16-bit signed integer
    I16,
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 128-bit signed integer
    I128,
    
    // Unsigned integers
    /// 8-bit unsigned integer
    U8,
    /// 16-bit unsigned integer
    U16,
    /// 32-bit unsigned integer
    U32,
    /// 64-bit unsigned integer
    U64,
    /// 128-bit unsigned integer
    U128,
    
    /// Boolean type
    Bool,
    /// String type
    Str,
    /// Unit type (void)
    Unit,
    /// Function type: (param_types) -> return_type
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    /// Error type (used to continue type checking after an error)
    Error,
}

impl Type {
    /// Check if this type is a signed integer
    pub fn is_signed_int(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128)
    }
    
    /// Check if this type is an unsigned integer
    pub fn is_unsigned_int(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128)
    }
    
    /// Check if this type is any integer type
    pub fn is_integer(&self) -> bool {
        self.is_signed_int() || self.is_unsigned_int()
    }
    
    /// Check if this type is numeric (can be used in arithmetic)
    pub fn is_numeric(&self) -> bool {
        self.is_integer()
    }
    
    /// Get the bit size of an integer type
    pub fn bit_size(&self) -> Option<u32> {
        match self {
            Type::I8 | Type::U8 => Some(8),
            Type::I16 | Type::U16 => Some(16),
            Type::I32 | Type::U32 => Some(32),
            Type::I64 | Type::U64 => Some(64),
            Type::I128 | Type::U128 => Some(128),
            _ => None,
        }
    }
    
    /// Check if this type is a boolean
    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }
    
    /// Check if this type is an error type
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }
    
    /// Check if types are compatible (for assignment, etc.)
    pub fn is_compatible(&self, other: &Type) -> bool {
        if self.is_error() || other.is_error() {
            return true; // Error types are compatible with anything to avoid cascading errors
        }
        self == other
    }
    
    /// Get the result type of a binary operation
    /// For now, both operands must be the same integer type
    pub fn binary_op_result(&self, op: &crate::ast::BinOp, other: &Type) -> Option<Type> {
        use crate::ast::BinOp;
        
        if self.is_error() || other.is_error() {
            return Some(Type::Error);
        }
        
        match op {
            // Arithmetic operators: same int type -> same int type
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if self.is_integer() && self == other {
                    Some(self.clone())
                } else {
                    None
                }
            }
            // Comparison operators: same int type -> bool
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if self.is_integer() && self == other {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            // Equality operators: T -> T -> bool (for comparable types)
            BinOp::Eq | BinOp::Ne => {
                if self == other || self.is_compatible(other) {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
            // Logical operators: bool -> bool -> bool
            BinOp::And | BinOp::Or => {
                if self.is_bool() && other.is_bool() {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }
    
    /// Get the result type of a unary operation
    pub fn unary_op_result(&self, op: &crate::ast::UnaryOp) -> Option<Type> {
        use crate::ast::UnaryOp;
        
        if self.is_error() {
            return Some(Type::Error);
        }
        
        match op {
            UnaryOp::Neg => {
                // Negation only valid for signed integers
                if self.is_signed_int() {
                    Some(self.clone())
                } else {
                    None
                }
            }
            UnaryOp::Not => {
                if self.is_bool() {
                    Some(Type::Bool)
                } else {
                    None
                }
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::I128 => write!(f, "i128"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U128 => write!(f, "u128"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Unit => write!(f, "()"),
            Type::Function { params, ret } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            }
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Parse a type from a string (for type annotations in source code)
pub fn parse_type_name(name: &str) -> Option<Type> {
    match name {
        "i8" => Some(Type::I8),
        "i16" => Some(Type::I16),
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "i128" => Some(Type::I128),
        "u8" => Some(Type::U8),
        "u16" => Some(Type::U16),
        "u32" => Some(Type::U32),
        "u64" => Some(Type::U64),
        "u128" => Some(Type::U128),
        "bool" => Some(Type::Bool),
        "str" => Some(Type::Str),
        _ => None,
    }
}
