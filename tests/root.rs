#[macro_use]
mod utils;

use eventree_wrapper::{
    self, ast_node,
    parser::{self, ParseConfig, SimpleTokens, TokenSet, Tokens},
};
use expect_test::expect;
use std::fmt;

enum CustomError {}

impl fmt::Display for CustomError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        unreachable!()
    }
}

type Parser<T> = parser::Parser<TreeConfig, T, CustomError>;

fn grammar_root<T: Tokens<TokenKind = TokenKind>>(p: &mut Parser<T>) {
    let marker = p.start();
    if !p.is_at_end() {
        let _guard = p.expected("nothing");
        p.error_without_skipping();
    }
    p.complete(marker, NodeKind::Root);
}

configure! {
    TreeConfig;

    #[derive(logos::Logos)]
    TokenKind {
        #[error]
        Error,
    };
    NodeKind {
        Root,
        Error,
    };
}

type Cfg = TreeConfig;

ast_node!(<Cfg> Root);

impl ParseConfig for TreeConfig {
    const ERROR: Self::NodeKind = NodeKind::Error;

    fn is_trivia(_: &Self::TokenKind) -> bool {
        false
    }

    fn default_recovery_set() -> TokenSet<Self::TokenKind> {
        TokenSet::EMPTY
    }
}

#[test]
#[cfg(feature = "logos")]
fn empty() {
    let tokens = SimpleTokens::tokenize("");
    let result = Parser::parse("", &tokens, grammar_root);
    assert!(!result.has_errors());
    expect!["Root@0..0"].assert_eq(&format!("{result}"));
}
