#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]

#[cfg_attr(test, macro_use)]
extern crate num_derive;

mod error;
mod event;
mod marker;
mod parser;
mod result;
mod sink;
mod token_set;
mod tokens;
mod traits;

pub use self::parser::{ExpectedDropGuard, ExpectedKind, Grammar, Parser};
pub use error::ParseError;
pub use marker::{CompletedMarker, Marker};
pub use result::ParseResult;
pub use token_set::TokenSet;
pub use tokens::SimpleTokens;
pub use traits::{ParseConfig, Tokens};

pub use num_derive::{FromPrimitive, ToPrimitive};
pub use num_traits::{FromPrimitive, ToPrimitive};
