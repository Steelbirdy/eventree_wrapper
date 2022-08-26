#[macro_use]
mod utils;

use eventree_wrapper::parser::TokenSet;

configure! {
    ParseConfig;

    TokenKind {
        Error,
    };

    NodeKind {
        Root,
        Error,
    };
}

eventree_wrapper::ast_node! { <ParseConfig> EmptyNode => [] }
eventree_wrapper::ast_token! { <ParseConfig> EmptyToken => [] }

impl eventree_wrapper::parser::ParseConfig for ParseConfig {
    type Error = std::convert::Infallible;

    const ERROR_NODE_KIND: Self::NodeKind = NodeKind::Error;

    fn is_trivia(_kind: &Self::TokenKind) -> bool {
        false
    }

    fn default_recovery_set() -> TokenSet<Self::TokenKind> {
        TokenSet::EMPTY
    }
}
