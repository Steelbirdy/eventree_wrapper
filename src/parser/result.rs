use crate::parser::{ParseConfig, ParseError};
use eventree::SyntaxTree;
use std::fmt;

pub struct ParseResult<C: ParseConfig> {
    syntax_tree: SyntaxTree<C>,
    errors: Vec<ParseError<C>>,
}

impl<C: ParseConfig> ParseResult<C> {
    pub(crate) fn new(syntax_tree: SyntaxTree<C>, errors: Vec<ParseError<C>>) -> Self {
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
    pub fn errors(&self) -> &[ParseError<C>] {
        &self.errors
    }

    #[must_use]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    #[must_use]
    pub fn into_parts(self) -> (SyntaxTree<C>, Vec<ParseError<C>>) {
        (self.syntax_tree, self.errors)
    }
}

impl<C: ParseConfig> fmt::Debug for ParseResult<C>
where
    SyntaxTree<C>: fmt::Debug,
    ParseError<C>: fmt::Display,
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
