#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    U8,
    U32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::U8 => 1,
            Type::U32 => 4,
        }
    }
}
