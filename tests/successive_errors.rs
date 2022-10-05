use eventree::TextRange;
use eventree_wrapper::parser::{SimpleTokens};

#[derive(logos::Logos, Debug, Eq, PartialEq, Hash)]
enum TokenKind {
    #[token("$")]
    Dollar,
    #[error]
    Error,
}

#[test]
fn successive_errors_are_flattened() {
    fn range(start: u32, end: u32) -> TextRange {
        TextRange::new(start.into(), end.into())
    }

    let tokens: SimpleTokens<TokenKind> = SimpleTokens::tokenize("$x$xxx$");
    let mut iter = tokens.iter();
    assert_eq!(iter.next(), Some((range(0, 1), &TokenKind::Dollar)));
    assert_eq!(iter.next(), Some((range(1, 2), &TokenKind::Error)));
    assert_eq!(iter.next(), Some((range(2, 3), &TokenKind::Dollar)));
    assert_eq!(iter.next(), Some((range(3, 6), &TokenKind::Error)));
    assert_eq!(iter.next(), Some((range(6, 7), &TokenKind::Dollar)));
    assert_eq!(iter.next(), None);
}