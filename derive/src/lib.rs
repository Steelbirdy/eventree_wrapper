extern crate proc_macro;
use proc_macro::TokenStream;

use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, Result};

// /// Example of [function-like procedural macro][1].
// ///
// /// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#function-like-procedural-macros
// #[proc_macro]
// pub fn my_macro(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//
//     let tokens = quote! {
//         #input
//
//         struct Hello;
//     };
//
//     tokens.into()
// }
//
// /// Example of user-defined [derive mode macro][1]
// ///
// /// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-mode-macros
// #[proc_macro_derive(MyDerive)]
// pub fn my_derive(_input: TokenStream) -> TokenStream {
//     let tokens = quote! {
//         struct Hello;
//     };
//
//     tokens.into()
// }

fn error(message: &str) -> syn::Error {
    syn::Error::new(proc_macro2::Span::call_site(), message)
}

/// Annotates a `use` import or an enum declaration, and
/// sets the default ParseConfig to the annotated type in the current module.
///
/// If the annotated item is a `use` import, it must import exactly one type (the ParseConfig).
/// If the annotated item is an enum, it must have no variants.
///
/// # Examples
/// ```
/// # use eventree_wrapper_derive as eventree_wrapper;
///
/// #[eventree_wrapper::parse_config]
/// #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// enum ParseConfig {}
/// ```
///
/// ```
/// # use eventree_wrapper_derive as eventree_wrapper;
///
/// mod config {
///     #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
///     pub enum ParseConfig {}
/// }
///
/// #[eventree_wrapper::parse_config]
/// use config::ParseConfig;
/// ```
///
/// ```compile_fail
/// # use eventree_wrapper_derive as eventree_wrapper;
///
/// // compile error: `parse_config` should annotate a `use` import or an enum declaration
/// #[eventree_wrapper::parse_config]
/// #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// struct ParseConfig;
/// ```
#[proc_macro_attribute]
pub fn parse_config(args: TokenStream, input: TokenStream) -> TokenStream {
    if !args.is_empty() {
        let err: proc_macro2::TokenStream = error("`parse_config` does not accept any arguments")
            .into_compile_error()
            .into();
        let input: proc_macro2::TokenStream = input.into();
        return quote!(
            #input
            #err
        )
        .into();
    }

    let input = parse_macro_input!(input as syn::Item);
    let result = match &input {
        syn::Item::Use(item_use) => parse_config_use(item_use),
        syn::Item::Enum(item_enum) => parse_config_enum(item_enum),
        _ => Err(error("`parse_config` should annotate a `use` import or an enum declaration")),
    };

    let path = match result {
        Ok(path) => path,
        Err(err) => {
            let err = err.into_compile_error();
            return quote!(
                #input
                #err
            )
            .into();
        }
    };

    quote!(
        #input
        type __EventreeWrapperParseConfig__ = #path;
    )
    .into()
}

fn parse_config_use(item: &syn::ItemUse) -> Result<syn::Path> {
    fn validate_path(
        tree: &syn::UseTree,
    ) -> syn::Result<Punctuated<syn::PathSegment, syn::Token![::]>> {
        let (ident, mut segments) = match tree {
            syn::UseTree::Path(path) => {
                let rest = validate_path(&path.tree)?;
                (path.ident.clone(), rest)
            }
            syn::UseTree::Group(_) => {
                return Err(error("`use` item must import exactly one type"));
            }
            syn::UseTree::Glob(_) => {
                return Err(error("`use` item cannot be a glob import"));
            }
            syn::UseTree::Name(name) => (name.ident.clone(), Punctuated::new()),
            syn::UseTree::Rename(rename) => (rename.ident.clone(), Punctuated::new()),
        };

        segments.push(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        });
        Ok(segments)
    }

    let segments = validate_path(&item.tree)?;
    Ok(syn::Path {
        leading_colon: item.leading_colon,
        segments: segments.into_iter().rev().collect(),
    })
}

fn parse_config_enum(item: &syn::ItemEnum) -> Result<syn::Path> {
    if !item.variants.is_empty() {
        return Err(error("use an enum with no variants as a parse config"));
    }

    Ok(syn::Path {
        leading_colon: None,
        segments: Punctuated::from_iter([syn::PathSegment {
            ident: item.ident.clone(),
            arguments: syn::PathArguments::None,
        }]),
    })
}
