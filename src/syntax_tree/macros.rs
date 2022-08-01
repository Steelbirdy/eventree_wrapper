#[macro_export]
macro_rules! ast_node {
    (<$Config:ty> $vis:vis $kind:ident $(fn $field:ident = $func:ident($ty:ty) $(. $next:ident($($arg:expr)*))* $(-> { $actual:ty })?;)*) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis struct $kind($vis $crate::eventree::SyntaxNode<$Config>);

        impl $crate::syntax_tree::AstNode<$Config> for $kind {
            fn cast(node: $crate::eventree::SyntaxNode<$Config>, tree: &$crate::eventree::SyntaxTree<$Config>) -> Option<Self> {
                if node.kind(tree) == <$Config as $crate::eventree::TreeConfig>::NodeKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxNode<$Config> {
                self.0
            }
        }

        impl $kind {
            $(
            pub fn $field(self, tree: &$crate::eventree::SyntaxTree<$Config>) -> $crate::ast_node!(@ret $func $ty $(=> $actual)?) {
                $crate::syntax_tree::$func::<$Config, Self, $ty>(self, tree) $(. $next($($arg),*))*
            }
            )*
        }
    };
    // TODO: Allow nested enum AstNode types to be defined via macro
    (<$Config:ty> $vis:vis $kind:ident => [$($variant:ident($inner:ident)),+ $(,)?] $(fn $field:ident = $func:ident($ty:ty);)*) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis enum $kind {
            $(
            $variant($inner),
            )+
        }

        impl $crate::syntax_tree::AstNode<$Config> for $kind {
            #[allow(unreachable_patterns)]
            fn cast(node: $crate::eventree::SyntaxNode<$Config>, tree: &$crate::eventree::SyntaxTree<$Config>) -> Option<Self> {
                match node.kind(tree) {
                    $(
                    <$Config as $crate::eventree::TreeConfig>::NodeKind::$inner => {
                        $inner::cast(node, tree).map(Self::$variant)
                    }
                    )+
                    _ => None,
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxNode<$Config> {
                match self {
                    $(
                    Self::$variant(inner) => inner.syntax(),
                    )+
                }
            }
        }

        impl $kind {
            $crate::ast_node!(@funcs <$Config> self, Self [$($variant),+] [$($field = $func($ty)),*]);
        }
    };
    (@funcs <$Config:ty> $this:ident, $This:ident [$($variant:ident),+] [$f_field:ident = $f_func:ident($f_ty:ty) $(, $($rest:tt)*)?]) => {
        fn $f_field($this, tree: &$crate::eventree::SyntaxTree<$Config>) -> $crate::ast_node!(@ret $f_func $f_ty) {
            match $this {
                $(
                $This::$variant(inner) => $crate::syntax_tree::$f_func(inner, tree),
                )+
            }
        }

        $crate::ast_node!(@funcs <$Config> $this, $This [$($variant),+] [$($($rest)*)?]);
    };
    (@funcs <$Config:ty> $this:ident, $This:ident [$($variant:ident),*] []) => {};
    (@ret $func:ident $ty:ty => $actual:ty) => {
        $actual
    };
    (@ret nodes $ty:ty) => {
        impl Iterator<Item = $ty> + '_
    };
    (@ret node $ty:ty) => {
        Option<$ty>
    };
    (@ret tokens $ty:ty) => {
        impl Iterator<Item = $ty> + '_
    };
    (@ret token $ty:ty) => {
        Option<$ty>
    };
}

#[macro_export]
macro_rules! ast_token {
    (<$Config:ty> $vis:vis $kind:ident) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis struct $kind($vis $crate::eventree::SyntaxToken<$Config>);

        impl $crate::syntax_tree::AstToken<$Config> for $kind {
            fn cast(token: $crate::eventree::SyntaxToken<$Config>, tree: &$crate::eventree::SyntaxTree<$Config>) -> Option<Self> {
                if token.kind(tree) == <$Config as $crate::eventree::TreeConfig>::TokenKind::$kind {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxToken<$Config> {
                self.0
            }
        }
    };
    (<$Config:ty> $vis:vis $kind:ident => [$($variant:ident($inner:ident)),+ $(,)?]) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis enum $kind {
            $(
            $variant($inner),
            )+
        }

        impl $crate::syntax_tree::AstToken<$Config> for $kind {
            fn cast(token: $crate::eventree::SyntaxToken<$Config>, tree: &$crate::eventree::SyntaxTree<$Config>) -> Option<Self> {
                match token.kind(tree) {
                    $(
                    <$Config as $crate::eventree::TreeConfig>::TokenKind::$inner => {
                        $inner::cast(token, tree).map(Self::$variant)
                    }
                    )+
                    _ => None,
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxToken<$Config> {
                match self {
                    $(
                    Self::$variant(inner) => inner.syntax(),
                    )+
                }
            }
        }
    };
}
