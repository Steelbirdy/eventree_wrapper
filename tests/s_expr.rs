use eventree_wrapper::parser::{self, SimpleTokens, TokenSet};
use expect_test::{expect, Expect};
use logos::Logos;
use std::mem;

#[cfg(not(feature = "logos"))]
use eventree::TextSize;

type Parser<'t> = parser::Parser<ParseConfig, &'t SimpleTokens<TokenKind>>;

lazy_static::lazy_static! {
    static ref OPERATORS: TokenSet<TokenKind> = TokenSet::new([Plus, Star]);
}

fn root(parser: &mut Parser) {
    let marker = parser.start();
    s_expr(parser);
    parser.complete(marker, NodeKind::Root);
}

fn s_expr(parser: &mut Parser) {
    if parser.is_at(LParen) {
        parse_cons(parser);
    } else {
        parse_atom(parser);
    }
}

fn parse_cons(p: &mut Parser) {
    assert!(p.is_at(LParen));
    let marker = p.start();
    p.bump();
    p.expect(*OPERATORS);
    s_expr(p);
    s_expr(p);
    p.expect(RParen);
    p.complete(marker, NodeKind::Cons);
}

fn parse_atom(p: &mut Parser) {
    assert!(p.is_at(Int));
    let marker = p.start();
    p.bump();
    p.complete(marker, NodeKind::Atom);
}

#[derive(Logos, Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
enum TokenKind {
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("+")]
    Plus,

    #[token("*")]
    Star,

    #[regex(r"\d+")]
    Int,

    #[regex(r"[ \t\r\n]+")]
    Whitespace,

    #[error]
    Error,
}

#[allow(clippy::enum_glob_use)]
use TokenKind::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
enum NodeKind {
    Root,
    Atom,
    Cons,
    Error,
}

#[allow(clippy::cast_possible_truncation)]
unsafe impl eventree::SyntaxKind for TokenKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}

#[allow(clippy::cast_possible_truncation)]
unsafe impl eventree::SyntaxKind for NodeKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        mem::transmute(raw as u8)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum ParseConfig {}

impl eventree::TreeConfig for ParseConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
}

impl parser::ParseConfig for ParseConfig {
    type Error = std::convert::Infallible;

    const ERROR_NODE_KIND: Self::NodeKind = NodeKind::Error;

    fn is_trivia(kind: &Self::TokenKind) -> bool {
        *kind == Whitespace
    }

    fn default_recovery_set() -> TokenSet<Self::TokenKind> {
        TokenSet::from(RParen)
    }
}

#[cfg(feature = "logos")]
#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let tokens = SimpleTokens::tokenize(source);
    let result = Parser::parse(source, &tokens, root);
    expect.assert_debug_eq(&result);
}

#[cfg(not(feature = "logos"))]
#[allow(clippy::needless_pass_by_value)]
fn check(source: &str, expect: Expect) {
    let mut lexer = TokenKind::lexer(source);
    let mut kinds = Vec::new();
    let mut starts = Vec::new();
    while let Some(token) = lexer.next() {
        kinds.push(token);
        starts.push(TextSize::from(lexer.span().start as u32));
    }
    starts.push(TextSize::of(source));

    let tokens = SimpleTokens::new(kinds.into_boxed_slice(), starts.into_boxed_slice());
    let result = Parser::parse(source, &tokens, root);
    expect.assert_eq(&format!("{}", result));
}

#[test]
fn atom() {
    check(
        "1",
        expect![
            r#"
Root@0..1
  Atom@0..1
    Int@0..1 "1"
"#
        ],
    );
}

#[test]
fn single_cons() {
    check(
        "(+ 1 2)",
        expect![
            r#"
Root@0..7
  Cons@0..7
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    Atom@3..4
      Int@3..4 "1"
    Whitespace@4..5 " "
    Atom@5..6
      Int@5..6 "2"
    RParen@6..7 ")"
"#
        ],
    );
}

#[test]
fn nested_cons() {
    check(
        "(+ (* 2 3) (* 4 2))",
        expect![
            r#"
Root@0..19
  Cons@0..19
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    Cons@3..10
      LParen@3..4 "("
      Star@4..5 "*"
      Whitespace@5..6 " "
      Atom@6..7
        Int@6..7 "2"
      Whitespace@7..8 " "
      Atom@8..9
        Int@8..9 "3"
      RParen@9..10 ")"
    Whitespace@10..11 " "
    Cons@11..18
      LParen@11..12 "("
      Star@12..13 "*"
      Whitespace@13..14 " "
      Atom@14..15
        Int@14..15 "4"
      Whitespace@15..16 " "
      Atom@16..17
        Int@16..17 "2"
      RParen@17..18 ")"
    RParen@18..19 ")"
"#
        ],
    );
}
