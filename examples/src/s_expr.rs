use eventree_wrapper::{
    ast_node, ast_token,
    eventree::{SyntaxKind, TreeConfig},
    parser::{self, ParseResult, SimpleTokens, TokenSet},
};
use logos::Logos;

fn parse(source: &str) -> ParseResult<ParseConfig> {
    let tokens = SimpleTokens::tokenize(source);
    Parser::parse(source, &tokens, s_expr)
}

#[derive(Logos, Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum TokenKind {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[regex(r"\d+")]
    IntLiteral,
    #[regex(r"[ \t\r\n]+")]
    Whitespace,
    #[error]
    Error,
}

unsafe impl SyntaxKind for TokenKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        std::mem::transmute(raw as u8)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum NodeKind {
    Root,
    Cons,
    Int,
    Error,
}

unsafe impl SyntaxKind for NodeKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        std::mem::transmute(raw as u8)
    }
}

mod config {
    #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
    pub enum ParseConfig {}
}
#[eventree_wrapper::parse_config]
use self::config::ParseConfig;

impl TreeConfig for ParseConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
}

impl parser::ParseConfig for ParseConfig {
    type Error = std::convert::Infallible;

    const ERROR_NODE_KIND: Self::NodeKind = NodeKind::Error;

    fn is_trivia(kind: &Self::TokenKind) -> bool {
        *kind == TokenKind::Whitespace
    }

    fn default_recovery_set() -> TokenSet<Self::TokenKind> {
        TokenSet::new([TokenKind::RParen])
    }
}

type Parser<'t> = parser::Parser<ParseConfig, &'t SimpleTokens<TokenKind>>;

lazy_static::lazy_static! {
    static ref OPERATORS: TokenSet<TokenKind> = {
        const OPERATORS: [TokenKind; 4] = [
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
        ];
        TokenSet::new(OPERATORS)
    };

    static ref ATOM_START: TokenSet<TokenKind> = {
        const ATOM_START: [TokenKind; 1] = [
            TokenKind::IntLiteral,
        ];
        TokenSet::new(ATOM_START)
    };
}

fn s_expr(p: &mut Parser) {
    let marker = p.start();
    if !p.is_at_end() {
        expr(p);
    }
    if !p.is_at_end() {
        let _guard = p.expected("end");
        p.error_with_only_recovery_set(TokenSet::EMPTY);
    }
    p.complete(marker, NodeKind::Root);
}

fn expr(p: &mut Parser) {
    if p.is_at(TokenKind::LParen) {
        cons(p)
    } else if p.is_at(TokenKind::IntLiteral) {
        int(p)
    } else {
        let _guard = p.expected("expression");
        p.error();
    }
}

fn cons(p: &mut Parser) {
    assert!(p.is_at(TokenKind::LParen));
    let marker = p.start();
    p.bump();
    {
        let _guard = p.expected("operator");
        p.expect_without_skipping(*OPERATORS);
    }
    expr(p);
    expr(p);
    p.expect(TokenKind::RParen);
    p.complete(marker, NodeKind::Cons);
}

fn int(p: &mut Parser) {
    assert!(p.is_at(TokenKind::IntLiteral));
    let marker = p.start();
    p.bump();
    p.complete(marker, NodeKind::Int);
}

#[ast_node]
struct Root;

#[ast_node]
enum Expr {
    Cons(Cons),
    Int(Int),
}

#[ast_node]
struct Cons;

#[ast_node]
struct Int;

eventree_wrapper::node_funcs! {
    impl Root {
        fn expr = node(Expr);
    }

    impl Cons {
        fn op = token(Operator);
        fn lhs = node(Expr);
        fn rhs = nodes(Expr).nth(1) -> Option<Expr>;
    }

    impl Int {
        fn value = token(IntLiteral);
    }
}

#[ast_token]
enum Operator {
    Add(Plus),
    Sub(Minus),
    Mul(Star),
    Div(Slash),
}

#[ast_token]
struct Plus;

#[ast_token]
struct Minus;

#[ast_token]
struct Star;

#[ast_token]
struct Slash;

#[ast_token]
struct IntLiteral;

#[cfg(test)]
mod tests {
    use super::*;
    use eventree_wrapper::syntax_tree::AstNode;
    use expect_test::{expect, Expect};

    fn check(source: &str, expect: Expect) -> ParseResult<ParseConfig> {
        let result = parse(source);
        expect.assert_debug_eq(&result);
        result
    }

    #[test]
    fn empty() {
        let result = check("", expect!["Root@0..0\n"]);
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "");
    }

    #[test]
    fn int() {
        let result = check(
            "10",
            expect![
                r#"
Root@0..2
  Int@0..2
    IntLiteral@0..2 "10"
"#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "10");

        let int = match root.expr(tree) {
            Some(Expr::Int(x)) => x,
            _ => unreachable!(),
        };
        assert_eq!(int.text(tree), "10");
    }

    #[test]
    fn cons() {
        let result = check(
            "(+ 1 2)",
            expect![
                r#"
Root@0..7
  Cons@0..7
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    Int@3..4
      IntLiteral@3..4 "1"
    Whitespace@4..5 " "
    Int@5..6
      IntLiteral@5..6 "2"
    RParen@6..7 ")"
"#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "(+ 1 2)");

        let cons = match root.expr(tree) {
            Some(Expr::Cons(c)) => c,
            _ => unreachable!(),
        };

        assert!(matches!(cons.op(tree), Some(Operator::Add(_))));
        assert!(matches!(cons.lhs(tree), Some(Expr::Int(_))));
        assert!(matches!(cons.rhs(tree), Some(Expr::Int(_))));
    }

    #[test]
    fn nested_cons() {
        let result = check(
            "(+ (* 1 2) (/ 3 4))",
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
      Int@6..7
        IntLiteral@6..7 "1"
      Whitespace@7..8 " "
      Int@8..9
        IntLiteral@8..9 "2"
      RParen@9..10 ")"
    Whitespace@10..11 " "
    Cons@11..18
      LParen@11..12 "("
      Slash@12..13 "/"
      Whitespace@13..14 " "
      Int@14..15
        IntLiteral@14..15 "3"
      Whitespace@15..16 " "
      Int@16..17
        IntLiteral@16..17 "4"
      RParen@17..18 ")"
    RParen@18..19 ")"
"#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "(+ (* 1 2) (/ 3 4))");
    }

    #[test]
    fn missing_operator() {
        check(
            "(1 2)",
            expect![
                r#"
Root@0..5
  Cons@0..5
    LParen@0..1 "("
    Int@1..2
      IntLiteral@1..2 "1"
    Whitespace@2..3 " "
    Int@3..4
      IntLiteral@3..4 "2"
    RParen@4..5 ")"
error at 1: missing operator
"#
            ],
        );
    }

    #[test]
    fn operator_in_middle() {
        check(
            "(1 + 2)",
            expect![
                r#"
Root@0..7
  Cons@0..6
    LParen@0..1 "("
    Int@1..2
      IntLiteral@1..2 "1"
    Whitespace@2..3 " "
    Error@3..4
      Plus@3..4 "+"
    Whitespace@4..5 " "
    Error@5..6
      IntLiteral@5..6 "2"
  Error@6..7
    RParen@6..7 ")"
error at 1: missing operator
error at 3..4: expected expression but found Plus
error at 5..6: expected RParen but found IntLiteral
error at 6..7: expected end but found RParen
"#
            ],
        );
    }

    #[test]
    fn missing_lhs_and_rhs() {
        check(
            "(+ )",
            expect![
                r#"
Root@0..4
  Cons@0..4
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    RParen@3..4 ")"
error at 2: missing expression
error at 2: missing expression
"#
            ],
        );
    }

    #[test]
    fn missing_rhs() {
        check(
            "(+ 1 )",
            expect![
                r#"
Root@0..6
  Cons@0..6
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    Int@3..4
      IntLiteral@3..4 "1"
    Whitespace@4..5 " "
    RParen@5..6 ")"
error at 4: missing expression
"#
            ],
        );
    }

    #[test]
    fn extra_tokens() {
        check(
            "(+ 1 1) 1",
            expect![
                r#"
Root@0..9
  Cons@0..7
    LParen@0..1 "("
    Plus@1..2 "+"
    Whitespace@2..3 " "
    Int@3..4
      IntLiteral@3..4 "1"
    Whitespace@4..5 " "
    Int@5..6
      IntLiteral@5..6 "1"
    RParen@6..7 ")"
  Whitespace@7..8 " "
  Error@8..9
    IntLiteral@8..9 "1"
error at 8..9: expected end but found IntLiteral
"#
            ],
        );
    }
}
