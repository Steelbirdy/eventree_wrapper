use num_traits::{FromPrimitive, ToPrimitive};
use std::fmt;
use std::marker::PhantomData;

const CONVERSION_FAIL_MESSAGE: &str =
    "converting the enum into a `u64` failed, are there too many variants?";

pub struct TokenSet<T> {
    flags: u64,
    __phantom: PhantomData<fn(T)>,
}

impl<T> TokenSet<T> {
    pub const EMPTY: Self = Self::_new(0);
    pub const ALL: Self = Self::_new(u64::MAX);

    #[must_use]
    const fn _new(flags: u64) -> Self {
        Self {
            flags,
            __phantom: PhantomData,
        }
    }

    #[must_use]
    pub fn len(self) -> usize {
        self.flags.count_ones() as usize
    }

    #[must_use]
    pub fn is_empty(self) -> bool {
        self.flags == 0
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self::_new(self.flags | other.flags)
    }

    #[must_use]
    pub fn intersection(self, other: Self) -> Self {
        Self::_new(self.flags & other.flags)
    }

    #[must_use]
    pub fn difference(self, other: Self) -> Self {
        Self::_new(self.flags & !other.flags)
    }
}

impl<T> TokenSet<T>
where
    T: ToPrimitive,
{
    #[must_use]
    pub fn new<const N: usize>(kinds: [T; N]) -> Self {
        let mut flags = 0;
        for kind in kinds {
            flags |= mask(kind);
        }
        Self::_new(flags)
    }

    #[must_use]
    pub fn contains(self, kind: T) -> bool {
        self.flags & mask(kind) != 0
    }

    #[must_use]
    pub fn with(self, kind: T) -> Self {
        Self::_new(self.flags | mask(kind))
    }

    #[must_use]
    pub fn without(self, kind: T) -> Self {
        Self::_new(self.flags & !mask(kind))
    }
}

impl<T> TokenSet<T>
where
    T: FromPrimitive,
{
    #[must_use]
    pub fn kinds(self) -> Kinds<T> {
        Kinds {
            flags: self.flags,
            index: 0,
            __phantom: PhantomData,
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
fn mask<T: ToPrimitive>(kind: T) -> u64 {
    let discriminant = kind.to_u64().expect(CONVERSION_FAIL_MESSAGE);
    1 << discriminant
}

impl<T: ToPrimitive> From<T> for TokenSet<T> {
    fn from(kind: T) -> Self {
        Self::new([kind])
    }
}

impl<T> Copy for TokenSet<T> {}

impl<T> Clone for TokenSet<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Eq for TokenSet<T> {}

impl<T> PartialEq for TokenSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.flags == other.flags
    }
}

impl<T> fmt::Debug for TokenSet<T>
where
    T: fmt::Debug + FromPrimitive,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.kinds()).finish()
    }
}

impl<T> std::hash::Hash for TokenSet<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.flags.hash(state);
    }
}

pub struct Kinds<T> {
    flags: u64,
    index: u64,
    __phantom: PhantomData<fn(T)>,
}

impl<T> Kinds<T> {
    pub fn is_empty(&self) -> bool {
        self.flags == 0
    }
}

impl<T> Iterator for Kinds<T>
where
    T: FromPrimitive,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if Kinds::is_empty(self) {
            return None;
        }
        let trailing_zeros = self.flags.trailing_zeros();
        self.flags = self
            .flags
            .checked_shr(trailing_zeros + 1)
            .unwrap_or_default();
        self.index += u64::from(trailing_zeros) + 1;
        let value = T::from_u64(self.index - 1).expect(CONVERSION_FAIL_MESSAGE);
        Some(value)
    }
}

impl<T> ExactSizeIterator for Kinds<T>
where
    T: FromPrimitive,
{
    fn len(&self) -> usize {
        self.flags.count_ones().try_into().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, ToPrimitive, FromPrimitive)]
    enum Kind {
        V0,
        V1,
        V2,
        V3,
        V4,
        V5,
        V6,
        V7,
        V8,
        V9,
        V10,
        V11,
        V12,
        V13,
        V14,
        V15,
        V16,
        V17,
        V18,
        V19,
        V20,
        V21,
        V22,
        V23,
        V24,
        V25,
        V26,
        V27,
        V28,
        V29,
        V30,
        V31,
        V32,
        V33,
        V34,
        V35,
        V36,
        V37,
        V38,
        V39,
        V40,
        V41,
        V42,
        V43,
        V44,
        V45,
        V46,
        V47,
        V48,
        V49,
        V50,
        V51,
        V52,
        V53,
        V54,
        V55,
        V56,
        V57,
        V58,
        V59,
        V60,
        V61,
        V62,
        V63,
    }

    #[test]
    fn kinds_with_empty_set() {
        let set = TokenSet::<Kind>::EMPTY;
        assert!(set.kinds().next().is_none());
    }

    #[test]
    fn kinds_with_only_last_variant() {
        let set = TokenSet::new([Kind::V63]);
        let mut kinds = set.kinds();
        assert_eq!(kinds.next(), Some(Kind::V63));
        assert!(kinds.next().is_none());
    }

    #[test]
    fn kinds_with_all() {
        let set = TokenSet::<Kind>::ALL;
        let mut kinds = set.kinds();
        for i in 0..64 {
            assert_eq!(format!("{:?}", kinds.next()), format!("Some(V{i})"));
        }
        assert!(kinds.next().is_none());
    }
}
