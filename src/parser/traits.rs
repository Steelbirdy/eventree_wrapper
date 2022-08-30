use crate::parser::TokenSet;
use eventree::TextRange;

pub trait ParseConfig: eventree::TreeConfig {
    type Error;

    const ERROR_NODE_KIND: Self::NodeKind;

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

mod __private {
    pub trait Sealed {}

    impl<T: eventree::SyntaxKind> Sealed for T {}

    impl<T: eventree::SyntaxKind> Sealed for crate::parser::TokenSet<T> {}
}

pub trait ParsePattern<T>: Sized + __private::Sealed {
    fn matches(&self, kind: T) -> bool;

    fn expected<C: ParseConfig<TokenKind = T>>(self) -> crate::parser::ExpectedKind<C>;
}

impl<T: eventree::SyntaxKind + PartialEq> ParsePattern<T> for T {
    fn matches(&self, kind: T) -> bool {
        *self == kind
    }

    fn expected<C: ParseConfig<TokenKind = T>>(self) -> crate::parser::ExpectedKind<C> {
        crate::parser::ExpectedKind::Unnamed(self)
    }
}

impl<T: eventree::SyntaxKind> ParsePattern<T> for TokenSet<T> {
    fn matches(&self, kind: T) -> bool {
        self.contains(kind)
    }

    fn expected<C: ParseConfig<TokenKind = T>>(self) -> crate::parser::ExpectedKind<C> {
        crate::parser::ExpectedKind::AnyUnnamed(self)
    }
}
