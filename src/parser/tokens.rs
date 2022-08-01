use crate::parser::Tokens;
use eventree::{TextRange, TextSize};

#[cfg(feature = "logos")]
use logos::{Logos, Source};

#[derive(Debug)]
pub struct SimpleTokens<T> {
    kinds: Box<[T]>,
    starts: Box<[TextSize]>,
}

impl<T> SimpleTokens<T> {
    #[must_use]
    pub fn new(kinds: Box<[T]>, starts: Box<[TextSize]>) -> Self {
        Self { kinds, starts }
    }
}

#[cfg(feature = "logos")]
impl<'a, T> SimpleTokens<T>
where
    T: Logos<'a>,
{
    #[must_use]
    pub fn tokenize(source: &'a T::Source) -> Self
    where
        T::Extras: Default,
    {
        Self::tokenize_impl(T::lexer(source))
    }

    #[must_use]
    pub fn tokenize_with_extras(source: &'a T::Source, extras: T::Extras) -> Self {
        Self::tokenize_impl(T::lexer_with_extras(source, extras))
    }

    #[allow(clippy::cast_possible_truncation)]
    fn tokenize_impl(lexer: logos::Lexer<'a, T>) -> Self {
        let len = lexer.source().len();
        let u32_max_as_usize: usize = u32::MAX.try_into().unwrap();

        assert!(len < u32_max_as_usize, "source is too long");

        let (kinds, mut starts): (Vec<_>, Vec<_>) = lexer
            .spanned()
            .map(|(kind, span)| {
                let start = TextSize::from(span.start as u32);
                (kind, start)
            })
            .unzip();
        starts.push(TextSize::from(len as u32));

        Self::new(kinds.into_boxed_slice(), starts.into_boxed_slice())
    }
}

impl<T> Tokens for SimpleTokens<T>
where
    T: Copy,
{
    type TokenKind = T;

    fn is_at_end(&self, index: usize) -> bool {
        index == self.kinds.len()
    }

    fn get_kind(&self, index: usize) -> Option<Self::TokenKind> {
        self.kinds.get(index).copied()
    }

    fn get_range(&self, index: usize) -> Option<TextRange> {
        let end = *self.starts.get(index + 1)?;
        let start = self.starts[index];
        Some(TextRange::new(start, end))
    }
}
