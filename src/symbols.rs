use std::{convert::TryInto, num::NonZeroUsize};

const TERM_MASK_BITS: usize = 1;
const TERM_MASK: usize = 0b1;

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Term {
    value: NonZeroUsize,
}

impl From<Variable> for Term {
    #[inline]
    fn from(variable: Variable) -> Self {
        Term {
            value: variable.value,
        }
    }
}

impl From<Constant> for Term {
    #[inline]
    fn from(constant: Constant) -> Self {
        Term {
            value: constant.value,
        }
    }
}

impl TryInto<Variable> for Term {
    type Error = ();

    fn try_into(self) -> Result<Variable, Self::Error> {
        if Variable::is(self) {
            Ok(Variable { value: self.value })
        } else {
            Err(())
        }
    }
}

impl TryInto<Constant> for Term {
    type Error = ();

    fn try_into(self) -> Result<Constant, Self::Error> {
        if Constant::is(self) {
            Ok(Constant { value: self.value })
        } else {
            Err(())
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    value: NonZeroUsize,
}

impl Variable {
    const MASK: usize = TERM_MASK;
    const MASK_BITS: usize = TERM_MASK_BITS;
    const TAG: usize = 0b1;
    const MAX: usize = usize::MAX >> Self::MASK_BITS;

    #[inline]
    pub fn is(t: Term) -> bool {
        t.value.get() & Self::MASK == Self::TAG
    }

    #[inline]
    pub fn to_index(self) -> usize {
        self.value.get() as usize >> Self::MASK_BITS
    }

    #[inline]
    pub fn from_index(index: usize) -> Self {
        debug_assert!(index <= Self::MAX);

        let value = unsafe {
            // SAFETY: The argument of `new_unchecked` is guaranteed to be
            // non-zero since `Self::TAG` is non-zero.
            NonZeroUsize::new_unchecked((index << Self::MASK_BITS) | Self::TAG)
        };

        Variable { value }
    }
}

impl string_interner::Symbol for Variable {
    #[inline]
    fn try_from_usize(index: usize) -> Option<Self> {
        Option::Some(Self::from_index(index))
    }

    #[inline]
    fn to_usize(self) -> usize {
        self.to_index()
    }
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        "_".to_owned() + &self.to_index().to_string()
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    value: NonZeroUsize,
}

impl Constant {
    const MASK: usize = TERM_MASK;
    const MASK_BITS: usize = TERM_MASK_BITS;
    const TAG: usize = 0b0;
    const MAX: usize = (usize::MAX >> Self::MASK_BITS) - 1;

    #[inline]
    pub fn is(t: Term) -> bool {
        t.value.get() & Self::MASK == Self::TAG
    }

    #[inline]
    pub fn to_index(self) -> usize {
        (self.value.get() as usize - 1) >> Self::MASK_BITS
    }

    #[inline]
    pub fn from_index(index: usize) -> Self {
        debug_assert!(index <= Self::MAX);
        let value = unsafe {
            // SAFETY: The argument of `new_unchecked` is guaranteed to be
            // non-zero since `Self::TAG` is non-zero.
            // TODO: Consider using `unchecked_math` and `unchecked_add` instead of `wrapping_add`.
            NonZeroUsize::new_unchecked((index.wrapping_add(1) << Self::MASK_BITS) | Self::TAG)
        };
        Constant { value }
    }
}

impl string_interner::Symbol for Constant {
    #[inline]
    fn try_from_usize(index: usize) -> Option<Self> {
        Option::Some(Self::from_index(index))
    }

    #[inline]
    fn to_usize(self) -> usize {
        self.to_index()
    }
}

struct Predicate(NonZeroUsize);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disjoint() {
        for term in 1..32usize {
            let t = Term {
                value: NonZeroUsize::new(term).unwrap(),
            };
            assert!(Variable::is(t) ^ Constant::is(t))
        }
    }
}
