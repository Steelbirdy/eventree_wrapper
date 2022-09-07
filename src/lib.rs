#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]

pub mod parser;
pub mod syntax_tree;

pub use eventree;

pub use eventree_wrapper_derive::parse_config;
