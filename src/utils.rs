use std::fmt::Debug;

use index_vec::{Idx, IndexSlice};

#[derive(Clone, Copy)]
pub struct FmtEnumerated<'a, I: Debug + Idx, T: Debug>(pub &'a IndexSlice<I, [T]>);
impl<I: Debug + Idx, T: Debug> Debug for FmtEnumerated<'_, I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.0.iter_enumerated()).finish()
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
