#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    PU8,
    PU16,
    PU32,
    PU64,
    PI8,
    PI16,
    PI32,
    PI64,
    Char,
    Array { ty: Box<Type>, count: u64 },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U8 | Type::I8 | Type::Char => 1,
            Type::U16 | Type::I16 => 2,
            Type::U32 | Type::I32 => 4,
            Type::U64
            | Type::I64
            | Type::PU8
            | Type::PI8
            | Type::PU16
            | Type::PI16
            | Type::PU32
            | Type::PI32
            | Type::PU64
            | Type::PI64 => 8,
            Type::Array { ty, .. } => ty.size(),
        }
    }

    pub fn pointer_to(&self) -> Self {
        match self {
            Type::U8 => Type::PU8,
            Type::U16 => Type::PU16,
            Type::U32 => Type::PU32,
            Type::U64 => Type::PU64,
            Type::I8 => Type::PI8,
            Type::I16 => Type::PI16,
            Type::I32 => Type::PI32,
            Type::I64 => Type::PI64,
            Type::Char => Type::PU8,
            _ => panic!("Cannot take pointer of type {:?}", self),
        }
    }

    pub fn value_at(&self) -> Self {
        match self {
            Type::PU8 => Type::U8,
            Type::PU16 => Type::U16,
            Type::PU32 => Type::U32,
            Type::PU64 => Type::U64,
            Type::PI8 => Type::I8,
            Type::PI16 => Type::I16,
            Type::PI32 => Type::I32,
            Type::PI64 => Type::I64,
            Type::Array { ty, .. } => *ty.clone(),
            _ => panic!("Cannot dereference type {:?}", self),
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::Char
        )
    }

    pub fn is_ptr(&self) -> bool {
        matches!(
            self,
            Type::PU8
                | Type::PU16
                | Type::PU32
                | Type::PU64
                | Type::PI8
                | Type::PI16
                | Type::PI32
                | Type::PI64
        )
    }
}
