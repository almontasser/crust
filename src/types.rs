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
    Char,
    Array { ty: Box<Type>, count: u64 },
    Pointer { ty: Box<Type>, count: u64 },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U8 | Type::I8 | Type::Char => 1,
            Type::U16 | Type::I16 => 2,
            Type::U32 | Type::I32 => 4,
            Type::U64 | Type::I64 | Type::Pointer { .. } => 8,
            Type::Array { ty, .. } => ty.size(),
        }
    }

    pub fn pointer_to(&self) -> Self {
        match self {
            Type::U8 => Type::Pointer {
                ty: Box::new(Type::U8),
                count: 1,
            },
            Type::U16 => Type::Pointer {
                ty: Box::new(Type::U16),
                count: 1,
            },
            Type::U32 => Type::Pointer {
                ty: Box::new(Type::U32),
                count: 1,
            },
            Type::U64 => Type::Pointer {
                ty: Box::new(Type::U64),
                count: 1,
            },
            Type::I8 => Type::Pointer {
                ty: Box::new(Type::I8),
                count: 1,
            },
            Type::I16 => Type::Pointer {
                ty: Box::new(Type::I16),
                count: 1,
            },
            Type::I32 => Type::Pointer {
                ty: Box::new(Type::I32),
                count: 1,
            },
            Type::I64 => Type::Pointer {
                ty: Box::new(Type::I64),
                count: 1,
            },
            Type::Char => Type::Pointer {
                ty: Box::new(Type::Char),
                count: 1,
            },
            Type::Pointer { ty, count } => Type::Pointer {
                ty: ty.clone(),
                count: *count + 1,
            },
            _ => panic!("Cannot take pointer of type {:?}", self),
        }
    }

    pub fn value_at(&self) -> Self {
        match self {
            Type::Pointer { ty, count } => {
                if *count == 1 {
                    *ty.clone()
                } else {
                    Type::Pointer {
                        ty: ty.clone(),
                        count: *count - 1,
                    }
                }
            }
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
        // should array be here?
        matches!(self, Type::Pointer { .. } | Type::Array { .. })
    }
}
