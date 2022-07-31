use crate::{error::ParseError, traits::ParseConfig};
use eventree::SyntaxTree;
use std::fmt;

pub struct ParseResult<C: ParseConfig, E> {
    syntax_tree: SyntaxTree<C>,
    errors: Vec<ParseError<C, E>>,
}

impl<C: ParseConfig, E> ParseResult<C, E> {
    pub(crate) fn new(syntax_tree: SyntaxTree<C>, errors: Vec<ParseError<C, E>>) -> Self {
        Self {
            syntax_tree,
            errors,
        }
    }

    #[must_use]
    pub fn syntax_tree(&self) -> &SyntaxTree<C> {
        &self.syntax_tree
    }

    #[must_use]
    pub fn into_syntax_tree(self) -> SyntaxTree<C> {
        self.syntax_tree
    }

    #[must_use]
    pub fn errors(&self) -> &[ParseError<C, E>] {
        &self.errors
    }

    #[must_use]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    #[must_use]
    pub fn into_parts(self) -> (SyntaxTree<C>, Vec<ParseError<C, E>>) {
        (self.syntax_tree, self.errors)
    }
}

impl<C: ParseConfig, E> fmt::Display for ParseResult<C, E>
where
    SyntaxTree<C>: fmt::Debug,
    ParseError<C, E>: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.syntax_tree);
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{error}")?;
        }

        Ok(())
    }
}
