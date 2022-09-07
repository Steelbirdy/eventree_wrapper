#[macro_export(local_inner_macros)]
macro_rules! pconfig {
    () => {
        __EventreeWrapperParseConfig__
    };
}

#[macro_export]
macro_rules! node_funcs {
    ($(impl $ty:ty {
        $(
        fn $func:ident = $inner:ident($inner_ty:ty) $(.$next:ident($($next_arg:expr),* $(,)?))* $(-> $ret:ty)?;
        )+
    })+) => {$(
        impl $ty {
            $(
            pub fn $func(self, tree: &$crate::eventree::SyntaxTree<$crate::pconfig!()>) -> $crate::node_funcs!(@ret $inner $inner_ty $(=> $ret)?) {
                $crate::syntax_tree::$inner::<$crate::pconfig!(), Self, $inner_ty>(self, tree) $(. $next($($next_arg),*))*
            }
            )*
        }
    )+};
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
