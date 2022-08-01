mod macros;

use eventree::{SyntaxNode, SyntaxToken, SyntaxTree, TextRange, TreeConfig};

pub trait AstNode<C: TreeConfig>: Copy + Sized {
    fn cast(node: SyntaxNode<C>, tree: &SyntaxTree<C>) -> Option<Self>;

    fn syntax(self) -> SyntaxNode<C>;

    fn text(self, tree: &SyntaxTree<C>) -> &str {
        self.syntax().text(tree)
    }

    fn range(self, tree: &SyntaxTree<C>) -> TextRange {
        self.syntax().range(tree)
    }
}

pub trait AstToken<C: TreeConfig>: Copy + Sized {
    fn cast(token: SyntaxToken<C>, tree: &SyntaxTree<C>) -> Option<Self>;

    fn syntax(self) -> SyntaxToken<C>;

    fn text(self, tree: &SyntaxTree<C>) -> &str {
        self.syntax().text(tree)
    }

    fn range(self, tree: &SyntaxTree<C>) -> TextRange {
        self.syntax().range(tree)
    }
}

pub fn nodes<C, Parent, Child>(
    node: Parent,
    tree: &SyntaxTree<C>,
) -> impl Iterator<Item = Child> + '_
where
    C: TreeConfig,
    Parent: AstNode<C>,
    Child: AstNode<C>,
{
    node.syntax()
        .child_nodes(tree)
        .filter_map(|node| Child::cast(node, tree))
}

pub fn node<C, Parent, Child>(node: Parent, tree: &SyntaxTree<C>) -> Option<Child>
where
    C: TreeConfig,
    Parent: AstNode<C>,
    Child: AstNode<C>,
{
    node.syntax()
        .child_nodes(tree)
        .find_map(|node| Child::cast(node, tree))
}

pub fn tokens<C, Node, Token>(node: Node, tree: &SyntaxTree<C>) -> impl Iterator<Item = Token> + '_
where
    C: TreeConfig,
    Node: AstNode<C>,
    Token: AstToken<C>,
{
    node.syntax()
        .child_tokens(tree)
        .filter_map(|tok| Token::cast(tok, tree))
}

pub fn token<C, Node, Token>(node: Node, tree: &SyntaxTree<C>) -> Option<Token>
where
    C: TreeConfig,
    Node: AstNode<C>,
    Token: AstToken<C>,
{
    node.syntax()
        .child_tokens(tree)
        .find_map(|tok| Token::cast(tok, tree))
}
