#![allow(dead_code)]

use std::fmt::{self, Debug};

use index_vec::{Idx, IndexSlice};

#[macro_export]
macro_rules! define_idx_type {
    {
        $(#[$($xs:tt)*])*
        name: $name:ident,
        debug_fmt: $debug_fmt:literal $(,)?
    } => {
        $(#[$($xs)*])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub usize);
        impl Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, $debug_fmt, self.0)
            }
        }
        impl index_vec::Idx for $name {
            fn from_usize(idx: usize) -> Self {
                Self(idx)
            }
            fn index(self) -> usize {
                self.0
            }
        }
    };
}

#[repr(transparent)]
pub struct FmtEnumerated<I: Debug + Idx, T: Debug>(IndexSlice<I, [T]>);
impl<I: Debug + Idx, T: Debug> FmtEnumerated<I, T> {
    pub const fn new(slice: &IndexSlice<I, [T]>) -> &Self {
        unsafe { std::mem::transmute(slice) }
    }
}
impl<I: Debug + Idx, T: Debug> Debug for FmtEnumerated<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.0.iter_enumerated()).finish()
    }
}

pub trait FmtEnumeratedTrait<I: Debug + Idx, T: Debug> {
    /// Debug format an `IndexSlice` as key-value pairs.
    fn fmt_enumerated(&self) -> &FmtEnumerated<I, T>;
}
impl<I: Debug + Idx, T: Debug> FmtEnumeratedTrait<I, T> for IndexSlice<I, [T]> {
    fn fmt_enumerated(&self) -> &FmtEnumerated<I, T> {
        FmtEnumerated::new(self)
    }
}

pub trait IndexVecUncheckedGet<I: Idx, T> {
    unsafe fn get_unchecked(&self, idx: I) -> &T;
    unsafe fn get_unchecked_mut(&mut self, idx: I) -> &mut T;
}
impl<I: Idx, T> IndexVecUncheckedGet<I, T> for IndexSlice<I, [T]> {
    unsafe fn get_unchecked(&self, idx: I) -> &T {
        self.raw.get_unchecked(idx.index())
    }

    unsafe fn get_unchecked_mut(&mut self, idx: I) -> &mut T {
        self.raw.get_unchecked_mut(idx.index())
    }
}
