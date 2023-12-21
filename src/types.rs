#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    U8,
    U32,
    PInt,
    PU8,
    PU32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::U8 => 1,
            Type::U32 => 4,
            Type::PInt => 8,
            Type::PU8 => 8,
            Type::PU32 => 8,
        }
    }

    pub fn pointer_to(&self) -> Self {
        match self {
            Type::Int => Type::PInt,
            Type::U8 => Type::PU8,
            Type::U32 => Type::PU32,
            _ => panic!("Cannot take pointer of type {:?}", self),
        }
    }

    pub fn value_at(&self) -> Self {
        match self {
            Type::PInt => Type::Int,
            Type::PU8 => Type::U8,
            Type::PU32 => Type::U32,
            _ => panic!("Cannot dereference type {:?}", self),
        }
    }
}
