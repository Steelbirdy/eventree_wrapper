use eventree_wrapper::{
    ast_node, ast_token,
    eventree::{SyntaxKind, TreeConfig},
    parser::{self, CompletedMarker, ParseResult, SimpleTokens},
};
use logos::Logos;
use std::fmt;

pub fn parse(source: &str) -> ParseResult<ParseConfig, ParseError> {
    let tokens = SimpleTokens::tokenize(source);
    Parser::parse(source, &tokens, expression)
}

fn expression(p: &mut Parser) {
    let marker = p.start();
    if !p.is_at_end() {
        expr(p, BindPower::MIN, TokenSet::EMPTY);
    }
    p.complete(marker, NodeKind::Root);
}

fn expr(p: &mut Parser, min_bp: BindPower, recovery_set: TokenSet) -> Option<CompletedMarker> {
    let mut lhs = expr_lhs(p)?;

    loop {
        if let Some((l_bp, r_bp)) = p.peek().as_ref().and_then(TokenKind::infix_bp) {
            if l_bp < min_bp {
                break;
            }
            p.bump();

            let marker = p.precede(lhs);
            let _guard = p.expected("operand");
            expr(p, r_bp, recovery_set);
            lhs = p.complete(marker, NodeKind::InfixExpr);
            continue;
        }

        break;
    }

    Some(lhs)
}

fn expr_lhs(p: &mut Parser) -> Option<CompletedMarker> {
    if p.is_at_any(*PREFIX_OP) {
        prefix(p)
    } else if p.is_at(TokenKind::LParen) {
        grouping(p)
    } else if p.is_at(TokenKind::IntLiteral) {
        int(p)
    } else {
        let _guard = p.expected("expression");
        p.error()
    }
}

fn prefix(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.is_at_any(*PREFIX_OP));
    let marker = p.start();
    p.bump();
    let (_, rbp) = TokenKind::PREFIX_BP;
    expr(p, rbp, TokenSet::EMPTY);
    Some(p.complete(marker, NodeKind::PrefixExpr))
}

fn grouping(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.is_at(TokenKind::LParen));
    let marker = p.start();
    p.bump();
    expr(p, BindPower::MIN, TokenSet::from(TokenKind::RParen));
    Some(p.complete(marker, NodeKind::Grouping))
}

fn int(p: &mut Parser) -> Option<CompletedMarker> {
    assert!(p.is_at(TokenKind::IntLiteral));
    let marker = p.start();
    p.bump();
    Some(p.complete(marker, NodeKind::Int))
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

type BindPower = u8;

impl TokenKind {
    const ADD_BP: (BindPower, BindPower) = (1, 2);
    const MUL_BP: (BindPower, BindPower) = {
        let (lbp, rbp) = Self::ADD_BP;
        (lbp + 2, rbp + 2)
    };
    const PREFIX_BP: ((), BindPower) = {
        let (_, rbp) = Self::MUL_BP;
        ((), rbp + 2)
    };

    fn infix_bp(&self) -> Option<(BindPower, BindPower)> {
        match self {
            Self::Plus | Self::Minus => Some(Self::ADD_BP),
            Self::Star | Self::Slash => Some(Self::MUL_BP),
            _ => None,
        }
    }
}

unsafe impl SyntaxKind for TokenKind {
    fn to_raw(self) -> u16 {
        self as u16
    }

    unsafe fn from_raw(raw: u16) -> Self {
        std::mem::transmute(raw as u8)
    }
}

type TokenSet = parser::TokenSet<TokenKind>;

lazy_static::lazy_static! {
    static ref PREFIX_OP: TokenSet = {
        TokenSet::new([TokenKind::Minus])
    };

    static ref INFIX_OP: TokenSet = {
        TokenSet::new([
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash
        ])
    };
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum NodeKind {
    Root,
    Grouping,
    PrefixExpr,
    InfixExpr,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ParseConfig {}

impl TreeConfig for ParseConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;
}

impl parser::ParseConfig for ParseConfig {
    const ERROR: Self::NodeKind = NodeKind::Error;

    fn is_trivia(kind: &Self::TokenKind) -> bool {
        *kind == TokenKind::Whitespace
    }

    fn default_recovery_set() -> parser::TokenSet<Self::TokenKind> {
        TokenSet::EMPTY
    }
}

type Parser<'a, 'b> = parser::Parser<'a, ParseConfig, &'b SimpleTokens<TokenKind>, ParseError>;

pub enum ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unreachable!()
    }
}

type Cfg = ParseConfig;

ast_node! { <Cfg> Root
    fn expr = node(Expr);
}
ast_node! { <Cfg> Expr => [Grouping(Grouping), Prefix(PrefixExpr), Infix(InfixExpr), Int(Int)] }
ast_node! { <Cfg> Grouping
    fn expr = node(Expr);
}
ast_node! { <Cfg> PrefixExpr
    fn op = token(PrefixOp);
    fn rhs = node(Expr);
}
ast_node! { <Cfg> InfixExpr
    fn op = token(InfixOp);
    fn lhs = node(Expr);
    fn rhs = nodes(Expr).nth(1) -> { Option<Expr> };
}
ast_node! { <Cfg> Int
    fn value = token(IntLiteral);
}

ast_token! { <Cfg> PrefixOp => [Neg(Minus)] }
ast_token! { <Cfg> InfixOp => [Add(Plus), Sub(Minus), Mul(Star), Div(Slash)] }
ast_token! { <Cfg> Plus }
ast_token! { <Cfg> Minus }
ast_token! { <Cfg> Star }
ast_token! { <Cfg> Slash }
ast_token! { <Cfg> IntLiteral }

#[cfg(test)]
mod tests {
    use super::*;
    use eventree_wrapper::syntax_tree::{AstNode, AstToken};
    use expect_test::{expect, Expect};

    fn check(source: &str, expect: Expect) -> ParseResult<ParseConfig, ParseError> {
        let parse_result = parse(source);
        expect.assert_eq(&format!("{parse_result}"));
        parse_result
    }

    #[test]
    fn empty() {
        let result = check("", expect!["Root@0..0"]);
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "");
    }

    #[test]
    fn int_literal() {
        let result = check(
            "107",
            expect![
                r#"
Root@0..3
  Int@0..3
    IntLiteral@0..3 "107""#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "107");

        let int = match root.expr(tree) {
            Some(Expr::Int(int)) => int,
            _ => unreachable!(),
        };
        assert_eq!(int.text(tree), "107");
    }

    #[test]
    fn prefix_expr() {
        let result = check(
            "-73",
            expect![
                r#"
Root@0..3
  PrefixExpr@0..3
    Minus@0..1 "-"
    Int@1..3
      IntLiteral@1..3 "73""#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "-73");

        let expr = match root.expr(tree) {
            Some(Expr::Prefix(p)) => p,
            _ => unreachable!(),
        };
        assert!(matches!(expr.op(tree), Some(PrefixOp::Neg(_))));
        assert!(matches!(expr.rhs(tree), Some(Expr::Int(int)) if int.text(tree) == "73"));
    }

    #[test]
    fn simple_infix_expr() {
        let result = check(
            "2 + 2",
            expect![
                r#"
Root@0..5
  InfixExpr@0..5
    Int@0..1
      IntLiteral@0..1 "2"
    Whitespace@1..2 " "
    Plus@2..3 "+"
    Whitespace@3..4 " "
    Int@4..5
      IntLiteral@4..5 "2""#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "2 + 2");

        let expr = match root.expr(tree) {
            Some(Expr::Infix(i)) => i,
            _ => unreachable!(),
        };
        assert!(matches!(expr.op(tree), Some(InfixOp::Add(_))));
        assert!(matches!(expr.lhs(tree), Some(Expr::Int(_))));
        assert!(matches!(expr.rhs(tree), Some(Expr::Int(_))));
    }

    #[test]
    fn compound_infix_expr() {
        let result = check(
            "2 * 3 - 6",
            expect![
                r#"
Root@0..9
  InfixExpr@0..9
    InfixExpr@0..5
      Int@0..1
        IntLiteral@0..1 "2"
      Whitespace@1..2 " "
      Star@2..3 "*"
      Whitespace@3..4 " "
      Int@4..5
        IntLiteral@4..5 "3"
    Whitespace@5..6 " "
    Minus@6..7 "-"
    Whitespace@7..8 " "
    Int@8..9
      IntLiteral@8..9 "6""#
            ],
        );
        assert!(!result.has_errors());
        let tree = result.syntax_tree();
        let root = Root::cast(tree.root(), tree).unwrap();
        assert_eq!(root.text(tree), "2 * 3 - 6");
    }
}
