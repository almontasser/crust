#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    PU8,
    PU16,
    PU32,
    PU64,
    Array { ty: Box<Type>, count: u64 },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U8 => 1,
            Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::PU8 => 8,
            Type::PU16 => 8,
            Type::PU32 => 8,
            Type::PU64 => 8,
            Type::Array { ty, .. } => ty.size(),
        }
    }

    pub fn pointer_to(&self) -> Self {
        match self {
            Type::U8 => Type::PU8,
            Type::U16 => Type::PU16,
            Type::U32 => Type::PU32,
            Type::U64 => Type::PU64,
            _ => panic!("Cannot take pointer of type {:?}", self),
        }
    }

    pub fn value_at(&self) -> Self {
        match self {
            Type::PU8 => Type::U8,
            Type::PU16 => Type::U16,
            Type::PU32 => Type::U32,
            Type::PU64 => Type::U64,
            Type::Array { ty, .. } => *ty.clone(),
            _ => panic!("Cannot dereference type {:?}", self),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Type::PU8 | Type::PU16 | Type::PU32 | Type::PU64 => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Type::Array { .. } => true,
            _ => false,
        }
    }
}
