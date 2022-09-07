#[macro_export(local_inner_macros)]
macro_rules! pconfig {
    () => {
        __EventreeWrapperParseConfig__
    };
}

#[macro_export]
macro_rules! ast_node {
    ($vis:vis $kind:ident $(fn $field:ident = $func:ident($ty:ty) $(. $next:ident($($arg:expr)*))* $(-> $actual:ty)?;)*) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis struct $kind($vis $crate::eventree::SyntaxNode<$crate::pconfig!()>);

        impl $crate::syntax_tree::AstNode<$crate::pconfig!()> for $kind {
            fn cast(node: $crate::eventree::SyntaxNode<$crate::pconfig!()>, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> Option<Self> {
                if node.kind(tree) == <$crate::pconfig!() as $crate::eventree::TreeConfig>::NodeKind::$kind {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxNode<$crate::pconfig!()> {
                self.0
            }
        }

        impl $kind {
            $(
            pub fn $field(self, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> $crate::ast_node!(@ret $func $ty $(=> $actual)?) {
                $crate::syntax_tree::$func::<$crate::pconfig!(), Self, $ty>(self, tree) $(. $next($($arg),*))*
            }
            )*
        }
    };
    // TODO: Allow nested enum AstNode types to be defined via macro
    ($vis:vis $kind:ident => [$($($variant:ident($inner:ident)),+ $(,)?)?] $(fn $field:ident = $func:ident($ty:ty);)*) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis enum $kind {
            $($(
            $variant($inner),
            )+)?
        }

        impl $crate::syntax_tree::AstNode<$crate::pconfig!()> for $kind {
            #[allow(unreachable_patterns)]
            fn cast(node: $crate::eventree::SyntaxNode<$crate::pconfig!()>, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> Option<Self> {
                $($(
                if let Some(casted) = $inner::cast(node, tree) {
                    return Some(Self::$variant(casted));
                }
                )+)?
                None
            }

            fn syntax(self) -> $crate::eventree::SyntaxNode<$crate::pconfig!()> {
                match self {
                    $($(
                    Self::$variant(inner) => inner.syntax(),
                    )+)?
                }
            }
        }

        impl $kind {
            $crate::ast_node!(@funcs self, Self [$($($variant),+)?] [$($field = $func($ty)),*]);
        }
    };
    (@funcs $this:ident, $This:ident [$($variant:ident),*] [$f_field:ident = $f_func:ident($f_ty:ty) $(, $($rest:tt)*)?]) => {
        fn $f_field($this, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> $crate::ast_node!(@ret $f_func $f_ty) {
            match $this {
                $(
                $This::$variant(inner) => $crate::syntax_tree::$f_func(inner, tree),
                )*
            }
        }

        $crate::ast_node!(@funcs $this, $This [$($variant),*] [$($($rest)*)?]);
    };
    (@funcs $this:ident, $This:ident [$($variant:ident),*] []) => {};
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
    ($vis:vis $kind:ident) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis struct $kind($vis $crate::eventree::SyntaxToken<$crate::pconfig!()>);

        impl $crate::syntax_tree::AstToken<$crate::pconfig!()> for $kind {
            fn cast(token: $crate::eventree::SyntaxToken<$crate::pconfig!()>, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> Option<Self> {
                if token.kind(tree) == <$crate::pconfig!() as $crate::eventree::TreeConfig>::TokenKind::$kind {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn syntax(self) -> $crate::eventree::SyntaxToken<$crate::pconfig!()> {
                self.0
            }
        }
    };
    ($vis:vis $kind:ident => [$($($variant:ident($inner:ident)),+ $(,)?)?]) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash)]
        $vis enum $kind {
            $($(
            $variant($inner),
            )+)?
        }

        impl $crate::syntax_tree::AstToken<$crate::pconfig!()> for $kind {
            fn cast(token: $crate::eventree::SyntaxToken<$crate::pconfig!()>, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> Option<Self> {
                $($(
                if let Some(casted) = $inner::cast(token, tree) {
                    return Some(Self::$variant(casted));
                }
                )+)?
                None
            }

            fn syntax(self) -> $crate::eventree::SyntaxToken<$crate::pconfig!()> {
                match self {
                    $($(
                    Self::$variant(inner) => inner.syntax(),
                    )+)?
                }
            }
        }
    };
}
