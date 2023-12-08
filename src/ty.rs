use std::{
    fmt::{self, Debug},
    rc::Rc,
};

pub type Ty = Rc<Ty_>;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IntSize {
    _64,
    _32,
    _16,
    _8,
}
impl IntSize {
    /// Size in bytes.
    pub const fn size(self) -> u64 {
        match self {
            IntSize::_64 => 8,
            IntSize::_32 => 4,
            IntSize::_16 => 2,
            IntSize::_8 => 1,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Signess {
    Signed,
    Unsigned,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatSize {
    _64,
    _32,
}
impl FloatSize {
    /// Size in bytes.
    pub const fn size(self) -> u64 {
        match self {
            FloatSize::_64 => 8,
            FloatSize::_32 => 4,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty_ {
    Int(Signess, IntSize),
    Float(FloatSize),
    Bool,
    Tuple(Vec<Ty>),
    Ptr(Ty),
    Never,
}
impl Debug for Ty_ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(Signess::Unsigned, IntSize::_64) => write!(f, "u64"),
            Self::Int(Signess::Unsigned, IntSize::_32) => write!(f, "u32"),
            Self::Int(Signess::Unsigned, IntSize::_16) => write!(f, "u16"),
            Self::Int(Signess::Unsigned, IntSize::_8) =>  write!(f, "u8"),
            Self::Int(Signess::Signed  , IntSize::_64) => write!(f, "i64"),
            Self::Int(Signess::Signed  , IntSize::_32) => write!(f, "i32"),
            Self::Int(Signess::Signed  , IntSize::_16) => write!(f, "i16"),
            Self::Int(Signess::Signed  , IntSize::_8) =>  write!(f, "i8"),
            Self::Float(FloatSize::_64) => write!(f, "f64"),
            Self::Float(FloatSize::_32) => write!(f, "f32"),
            Self::Bool => write!(f, "bool"),
            Self::Tuple(tys) => {
                write!(f, "(")?;
                for ty in tys {
                    write!(f, "{:?},", ty)?;
                }
                write!(f, ")")
            }
            Self::Ptr(ty) => write!(f, "*{:?}", ty),
            Self::Never => write!(f, "never"),
        }
    }
}

impl Ty_ {
    /// Size of the type in number of bytes.
    pub fn size(&self) -> u64 {
        match self {
            Self::Int(_, size) => size.size(),
            Self::Float(size) => size.size(),
            Self::Bool => 1,
            Self::Tuple(tys) => tys.iter().map(|t| t.size()).sum(),
            Self::Ptr(..) => 8,
            Self::Never => 0,
        }
    }
}

/// The type of a function.
#[derive(Clone, PartialEq, Eq)]
pub struct FnType {
    pub inputs: Vec<Ty>,
    pub outputs: Ty,
}

impl Debug for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        for ty in &self.inputs {
            write!(f, "{:?},", ty)?;
        }
        write!(f, ") -> {:?}", self.outputs)
    }
}
