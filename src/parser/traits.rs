use crate::parser::TokenSet;
use eventree::TextRange;

pub trait ParseConfig: eventree::TreeConfig {
    const ERROR: Self::NodeKind;

    fn is_trivia(kind: &Self::TokenKind) -> bool;

    fn default_recovery_set() -> TokenSet<Self::TokenKind>;
}

pub trait Tokens {
    type TokenKind;

    fn is_at_end(&self, index: usize) -> bool;

    fn get_kind(&self, index: usize) -> Option<Self::TokenKind>;

    fn get_range(&self, index: usize) -> Option<TextRange>;

    #[inline]
    fn kind(&self, index: usize) -> Self::TokenKind {
        self.get_kind(index).unwrap()
    }

    #[inline]
    fn range(&self, index: usize) -> TextRange {
        self.get_range(index).unwrap()
    }
}

impl<T: Tokens> Tokens for &T {
    type TokenKind = T::TokenKind;

    #[inline]
    fn is_at_end(&self, index: usize) -> bool {
        T::is_at_end(self, index)
    }

    #[inline]
    fn get_kind(&self, index: usize) -> Option<Self::TokenKind> {
        T::get_kind(self, index)
    }

    #[inline]
    fn get_range(&self, index: usize) -> Option<TextRange> {
        T::get_range(self, index)
    }

    #[inline]
    fn kind(&self, index: usize) -> Self::TokenKind {
        T::kind(self, index)
    }

    #[inline]
    fn range(&self, index: usize) -> TextRange {
        T::range(self, index)
    }
}
