macro_rules! configure {
    ($Config:ident; $(#[$t_outer_meta:meta])* $TokenKind:ident { $($(#[$t_inner_meta:meta])* $t_kind:ident),* $(,)? }; $NodeKind:ident { $($n_kind:ident),* $(,)? };) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub enum $Config {}

        impl ::eventree_wrapper::eventree::TreeConfig for $Config {
            type NodeKind = $NodeKind;
            type TokenKind = $TokenKind;
        }

        $(#[$t_outer_meta])*
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #[repr(u8)]
        pub enum $TokenKind {
            $($(#[$t_inner_meta])* $t_kind,)*
        }

        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #[repr(u8)]
        pub enum $NodeKind {
            $($n_kind,)*
        }

        #[allow(clippy::cast_possible_truncation)]
        unsafe impl ::eventree_wrapper::eventree::SyntaxKind for $TokenKind {
            fn to_raw(self) -> u16 {
                self as u16
            }

            unsafe fn from_raw(raw: u16) -> Self {
                ::std::mem::transmute(raw as u8)
            }
        }

        #[allow(clippy::cast_possible_truncation)]
        unsafe impl ::eventree_wrapper::eventree::SyntaxKind for $NodeKind {
            fn to_raw(self) -> u16 {
                self as u16
            }

            unsafe fn from_raw(raw: u16) -> Self {
                ::std::mem::transmute(raw as u8)
            }
        }
    };
}
