extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::{quote, ToTokens};
use syn::{parse_macro_input, Result};

const PARSE_CONFIG_TYPE_ALIAS: &str = "__EventreeWrapperParseConfig__";

fn parse_config_ident() -> syn::Ident {
    syn::Ident::new(PARSE_CONFIG_TYPE_ALIAS, proc_macro2::Span::call_site())
}

/// TODO: Documentation
#[proc_macro_attribute]
pub fn ast_node(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    macro_rules! error_to_tokens {
        ($err:expr) => {{
            let err: TokenStream2 = $err.into_compile_error().into();
            quote!(
                #input
                #err
            ).into()
        }};
    }

    if !input.generics.params.is_empty() || input.generics.where_clause.is_some() {
        return error_to_tokens!(error("generic parameters/clauses are not supported"));
    }

    let result = match &input.data {
        syn::Data::Enum(data_enum) => ast_node_enum(&input, data_enum),
        syn::Data::Struct(data_struct) => ast_node_struct(&input, data_struct),
        syn::Data::Union(_) => Err(error("unions are not supported")),
    };

    match result {
        Ok(ret) => ret.into(),
        Err(err) => {
            let err = err.into_compile_error();
            quote!(#err).into()
        }
    }
}

fn ast_node_enum(input: &syn::DeriveInput, data: &syn::DataEnum) -> Result<TokenStream2> {
    let ident = &input.ident;

    // validation
    for variant in &data.variants {
        match &variant.fields {
            syn::Fields::Unnamed(unnamed) => {
                if unnamed.unnamed.len() != 1 {
                    return Err(error("enum tuple variant must have exactly 1 field"));
                }
            }
            syn::Fields::Named(_) => return Err(error("enum struct variants are not supported")),
            syn::Fields::Unit => {}
        }
    }

    let config = parse_config_ident();

    let mut variants = TokenStream2::new();
    let mut variant_checks = TokenStream2::new();
    let mut unwrap_variants = TokenStream2::new();
    for variant in &data.variants {
        let variant_ident = &variant.ident;
        let inner_ty = match &variant.fields {
            syn::Fields::Unnamed(unnamed) => unnamed.unnamed.first().map(|f| f.ty.clone()).unwrap(),
            syn::Fields::Unit => syn::Type::Verbatim(variant_ident.to_token_stream()),
            syn::Fields::Named(_) => unreachable!(),
        };

        variants = quote! {
            #variants
            #variant_ident(#inner_ty),
        };
        variant_checks = quote! {
            #variant_checks
            if let Some(casted) = #inner_ty::cast(node, tree) {
                return Some(Self::#variant_ident(casted));
            }
        };
        unwrap_variants = quote! {
            #unwrap_variants
            Self::#variant_ident(inner) => inner.syntax(),
        };
    }

    let enum_decl = {
        let vis = &input.vis;
        quote! {
            #[derive(Copy, Clone, Eq, PartialEq, Hash)]
            #vis enum #ident {
                #variants
            }
        }
    };

    Ok(quote!(
        #enum_decl

        impl ::eventree_wrapper::syntax_tree::AstNode<#config> for #ident {
            fn cast(node: ::eventree_wrapper::eventree::SyntaxNode<#config>, tree: &::eventree_wrapper::eventree::SyntaxTree<#config>) -> Option<Self> {
                #variant_checks
                None
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::SyntaxNode<#config> {
                match self {
                    #unwrap_variants
                }
            }
        }
    ).into())
}

fn ast_node_struct(input: &syn::DeriveInput, data: &syn::DataStruct) -> Result<TokenStream2> {
    let ident = &input.ident;

    if !data.fields.is_empty() {
        return Err(error("`ast_node` struct cannot have any fields"));
    }
    let config = parse_config_ident();

    let struct_decl = {
        let vis = &input.vis;
        quote! {
            #[derive(Copy, Clone, Eq, PartialEq, Hash)]
            #vis struct #ident(::eventree_wrapper::eventree::SyntaxNode<#config>);
        }
    };

    Ok(quote!(
        #struct_decl

        impl ::eventree_wrapper::syntax_tree::AstNode<#config> for #ident {
            fn cast(node: ::eventree_wrapper::eventree::SyntaxNode<#config>, tree: &::eventree_wrapper::eventree::SyntaxTree<#config>) -> Option<Self> {
                if node.kind(tree) == <#config as ::eventree_wrapper::eventree::TreeConfig>::NodeKind::#ident {
                    Some(Self(node))
                } else {
                    None
                }
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::SyntaxNode<#config> {
                self.0
            }
        }
    ).into())
}

/// TODO: Documentation
#[proc_macro_attribute]
pub fn ast_token(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    macro_rules! error_to_tokens {
        ($err:expr) => {{
            let err: TokenStream2 = $err.into_compile_error().into();
            quote!(
                #input
                #err
            ).into()
        }};
    }

    if !input.generics.params.is_empty() || input.generics.where_clause.is_some() {
        return error_to_tokens!(error("generic parameters/clauses are not supported"));
    }

    let result = match &input.data {
        syn::Data::Enum(data_enum) => ast_token_enum(&input, data_enum),
        syn::Data::Struct(data_struct) => ast_token_struct(&input, data_struct),
        syn::Data::Union(_) => Err(error("unions are not supported")),
    };

    match result {
        Ok(ret) => ret.into(),
        Err(err) => {
            let err = err.into_compile_error();
            quote!(#err).into()
        }
    }
}

fn ast_token_enum(input: &syn::DeriveInput, data: &syn::DataEnum) -> Result<TokenStream2> {
    let ident = &input.ident;

    // validation
    for variant in &data.variants {
        match &variant.fields {
            syn::Fields::Unnamed(unnamed) => {
                if unnamed.unnamed.len() != 1 {
                    return Err(error("enum tuple variant must have exactly 1 field"));
                }
            }
            syn::Fields::Named(_) => return Err(error("enum struct variants are not supported")),
            syn::Fields::Unit => {}
        }
    }

    let config = parse_config_ident();

    let mut variants = TokenStream2::new();
    let mut variant_checks = TokenStream2::new();
    let mut unwrap_variants = TokenStream2::new();
    for variant in &data.variants {
        let variant_ident = &variant.ident;
        let inner_ty = match &variant.fields {
            syn::Fields::Unnamed(unnamed) => unnamed.unnamed.first().map(|f| f.ty.clone()).unwrap(),
            syn::Fields::Unit => syn::Type::Verbatim(variant_ident.to_token_stream()),
            syn::Fields::Named(_) => unreachable!(),
        };

        variants = quote! {
            #variants
            #variant_ident(#inner_ty),
        };
        variant_checks = quote! {
            #variant_checks
            if let Some(casted) = #inner_ty::cast(token, tree) {
                return Some(Self::#variant_ident(casted));
            }
        };
        unwrap_variants = quote! {
            #unwrap_variants
            Self::#variant_ident(inner) => inner.syntax(),
        };
    }

    let enum_decl = {
        let vis = &input.vis;
        quote! {
            #[derive(Copy, Clone, Eq, PartialEq, Hash)]
            #vis enum #ident {
                #variants
            }
        }
    };

    Ok(quote!(
        #enum_decl

        impl ::eventree_wrapper::syntax_tree::AstToken<#config> for #ident {
            fn cast(token: ::eventree_wrapper::eventree::SyntaxToken<#config>, tree: &::eventree_wrapper::eventree::SyntaxTree<#config>) -> Option<Self> {
                #variant_checks
                None
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::SyntaxToken<#config> {
                match self {
                    #unwrap_variants
                }
            }
        }
    ).into())
}

fn ast_token_struct(input: &syn::DeriveInput, data: &syn::DataStruct) -> Result<TokenStream2> {
    let ident = &input.ident;

    if !data.fields.is_empty() {
        return Err(error("`ast_token` struct cannot have any fields"));
    }
    let config = parse_config_ident();

    let struct_decl = {
        let vis = &input.vis;
        quote! {
            #[derive(Copy, Clone, Eq, PartialEq, Hash)]
            #vis struct #ident(::eventree_wrapper::eventree::SyntaxToken<#config>);
        }
    };

    Ok(quote!(
        #struct_decl

        impl ::eventree_wrapper::syntax_tree::AstToken<#config> for #ident {
            fn cast(token: ::eventree_wrapper::eventree::SyntaxToken<#config>, tree: &::eventree_wrapper::eventree::SyntaxTree<#config>) -> Option<Self> {
                if token.kind(tree) == <#config as ::eventree_wrapper::eventree::TreeConfig>::TokenKind::#ident {
                    Some(Self(token))
                } else {
                    None
                }
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::SyntaxToken<#config> {
                self.0
            }
        }
    ).into())
}

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
    let config = parse_config_ident();

    let input = parse_macro_input!(input as syn::Item);
    let result = match &input {
        syn::Item::Use(item_use) => parse_config_use(item_use),
        syn::Item::Enum(item_enum) => parse_config_enum(item_enum),
        _ => Err(error(
            "`parse_config` should annotate a `use` import or an enum declaration",
        )),
    };

    let ident = match result {
        Ok(ident) => ident,
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
        type #config = #ident;
    )
    .into()
}

fn parse_config_use(item: &syn::ItemUse) -> Result<&syn::Ident> {
    let mut tree = &item.tree;
    loop {
        return match tree {
            syn::UseTree::Path(path) => {
                tree = &path.tree;
                continue;
            }
            syn::UseTree::Group(group) => {
                let items = &group.items;
                if items.len() == 1 {
                    tree = &items[0];
                    continue;
                } else {
                    Err(error("`use` item cannot be a group import of more than one item"))
                }
            }
            syn::UseTree::Glob(_) => Err(error("`use` item cannot be a glob import")),
            syn::UseTree::Name(name) => Ok(&name.ident),
            syn::UseTree::Rename(rename) => Ok(&rename.rename),
        };
    }
}

fn parse_config_enum(item: &syn::ItemEnum) -> Result<&syn::Ident> {
    if !item.variants.is_empty() {
        return Err(error("use an enum with no variants as a parse config"));
    }
    Ok(&item.ident)
}
