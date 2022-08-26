use eventree::SyntaxKind;
use std::fmt;
use std::marker::PhantomData;

pub struct TokenSet<T> {
    flags: u64,
    __phantom: PhantomData<fn(T)>,
}

impl<T> TokenSet<T> {
    pub const EMPTY: Self = Self::_new(0);

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
    T: SyntaxKind,
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
    pub fn with(mut self, kind: T) -> Self {
        self.insert(kind);
        self
    }

    #[must_use]
    pub fn without(mut self, kind: T) -> Self {
        self.remove(kind);
        self
    }

    pub fn insert(&mut self, kind: T) {
        self.flags |= mask(kind);
    }

    pub fn remove(&mut self, kind: T) {
        self.flags &= !mask(kind);
    }

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
fn mask<T: SyntaxKind>(kind: T) -> u64 {
    1_u64.checked_shl(kind.to_raw().into()).expect("")
}

impl<T: SyntaxKind> From<T> for TokenSet<T> {
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
    T: fmt::Debug + SyntaxKind,
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
    T: SyntaxKind,
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
        // SAFETY:
        // * Firstly, the `as` cast will never truncate because `index`
        //  is at most 64.
        // * The only way for a bit to be set in `flags` is for a variant
        //  with that discriminant to exist. Hence this is safe as long as
        // `SyntaxKind` is correctly implemented on `T`.
        #[allow(clippy::cast_possible_truncation)]
        let value = unsafe { T::from_raw(self.index as u16 - 1) };
        Some(value)
    }
}

impl<T> ExactSizeIterator for Kinds<T>
where
    T: SyntaxKind,
{
    fn len(&self) -> usize {
        self.flags.count_ones().try_into().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(unused)]
    #[derive(Debug, PartialEq)]
    #[repr(u8)]
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

    unsafe impl SyntaxKind for Kind {
        fn to_raw(self) -> u16 {
            self as u16
        }

        unsafe fn from_raw(raw: u16) -> Self {
            std::mem::transmute(raw as u8)
        }
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
    fn kinds_with_only_first_variant() {
        let set = TokenSet::new([Kind::V0]);
        let mut kinds = set.kinds();
        assert_eq!(kinds.next(), Some(Kind::V0));
        assert!(kinds.next().is_none());
    }

    #[test]
    fn kinds_with_all() {
        let set = TokenSet::<Kind>::_new(u64::MAX);
        let mut kinds = set.kinds();
        for i in 0..64 {
            assert_eq!(format!("{:?}", kinds.next()), format!("Some(V{i})"));
        }
        assert!(kinds.next().is_none());
    }

    #[test]
    fn insert() {
        let mut set = TokenSet::new([Kind::V10]);
        set.insert(Kind::V11);
        assert_eq!(set.kinds().collect::<Vec<_>>(), [Kind::V10, Kind::V11]);
    }

    #[test]
    fn remove() {
        let mut set = TokenSet::new([Kind::V0, Kind::V63]);
        set.remove(Kind::V0);
        assert_eq!(set.kinds().collect::<Vec<_>>(), [Kind::V63]);
    }

    #[test]
    fn kinds_len() {
        assert_eq!(TokenSet::<Kind>::EMPTY.kinds().len(), 0);
        assert_eq!(TokenSet::<Kind>::_new(u64::MAX).kinds().len(), 64);
        let set = TokenSet::new([Kind::V7, Kind::V61, Kind::V22]);
        assert_eq!(set.kinds().len(), 3);
    }
}
