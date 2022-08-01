use crate::parser::{ExpectedKind, ParseConfig, TokenSet};
use eventree::{TextRange, TextSize};
use std::fmt;

pub enum ParseError<C: ParseConfig, E> {
    Missing {
        expected: ExpectedKind<C>,
        offset: TextSize,
    },
    Unexpected {
        expected: ExpectedKind<C>,
        found: C::TokenKind,
        range: TextRange,
    },
    User(E),
}

impl<C: ParseConfig, E> From<E> for ParseError<C, E> {
    fn from(err: E) -> Self {
        Self::User(err)
    }
}

impl<C, E> fmt::Debug for ParseError<C, E>
where
    C: ParseConfig,
    E: fmt::Debug,
    ExpectedKind<C>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Missing { expected, offset } => f
                .debug_struct("Missing")
                .field("expected", expected)
                .field("offset", offset)
                .finish(),
            Self::Unexpected {
                expected,
                found,
                range,
            } => f
                .debug_struct("Unexpected")
                .field("expected", expected)
                .field("found", found)
                .field("range", range)
                .finish(),
            Self::User(err) => fmt::Debug::fmt(err, f),
        }
    }
}

impl<C, E> fmt::Display for ParseError<C, E>
where
    C: ParseConfig,
    E: fmt::Display,
    TokenSet<C::TokenKind>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Self::User(err) = self {
            return fmt::Display::fmt(err, f);
        }

        write!(f, "error at ")?;
        match self {
            Self::Missing { offset, .. } => {
                let offset = u32::from(*offset);
                write!(f, "{offset}")?;
            }
            Self::Unexpected { range, .. } => {
                let (start, end) = (u32::from(range.start()), u32::from(range.end()));
                write!(f, "{start}..{end}")?;
            }
            Self::User(_) => unreachable!(),
        };
        write!(f, ": ")?;

        let fmt_expected = |expected: &ExpectedKind<C>, f: &mut fmt::Formatter<'_>| match expected {
            ExpectedKind::Named(name) => write!(f, "{name}"),
            ExpectedKind::Unnamed(kind) => write!(f, "{kind:?}"),
            ExpectedKind::AnyUnnamed(set) => write!(f, "any of {set:?}"),
        };

        match self {
            Self::Missing { expected, .. } => {
                write!(f, "missing ")?;
                fmt_expected(expected, f)
            }
            Self::Unexpected {
                expected, found, ..
            } => {
                write!(f, "expected ")?;
                fmt_expected(expected, f)?;
                write!(f, " but found {found:?}")
            }
            Self::User(_) => unreachable!(),
        }
    }
}
