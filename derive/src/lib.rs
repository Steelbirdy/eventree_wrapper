extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};

use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Result};

macro_rules! ident {
    ($x:literal) => {
        syn::Ident::new($x, Span::call_site())
    };
}

fn parse_config_ident() -> syn::Ident {
    ident!["__EventreeWrapperParseConfig__"]
}

struct MacroConfig {
    trait_name: syn::Ident,
    ast_type_name: syn::Ident,
    assoc_type_name: syn::Ident,
}

impl MacroConfig {
    fn ast_node() -> Self {
        Self {
            trait_name: ident!["AstNode"],
            ast_type_name: ident!["SyntaxNode"],
            assoc_type_name: ident!["NodeKind"],
        }
    }

    fn ast_token() -> Self {
        Self {
            trait_name: ident!["AstToken"],
            ast_type_name: ident!["SyntaxToken"],
            assoc_type_name: ident!["TokenKind"],
        }
    }
}

/// TODO: Documentation
#[proc_macro_attribute]
pub fn ast_node(args: TokenStream, input: TokenStream) -> TokenStream {
    ast_macro(args, input, MacroConfig::ast_node())
}

/// TODO: Documentation
#[proc_macro_attribute]
pub fn ast_token(args: TokenStream, input: TokenStream) -> TokenStream {
    ast_macro(args, input, MacroConfig::ast_token())
}

fn ast_macro(_args: TokenStream, input: TokenStream, cfg: MacroConfig) -> TokenStream {
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
        syn::Data::Enum(data_enum) => ast_macro_enum(&input, data_enum, cfg),
        syn::Data::Struct(data_struct) => ast_macro_struct(&input, data_struct, cfg),
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

fn ast_macro_enum(
    input: &syn::DeriveInput,
    data: &syn::DataEnum,
    cfg: MacroConfig,
) -> Result<TokenStream2> {
    let MacroConfig {
        trait_name,
        ast_type_name,
        ..
    } = cfg;
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
            if let Some(casted) = #inner_ty::cast(x, tree) {
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

        impl ::eventree_wrapper::syntax_tree::#trait_name<#config> for #ident {
            fn cast(
                x: ::eventree_wrapper::eventree::#ast_type_name<#config>,
                tree: &::eventree_wrapper::eventree::SyntaxTree<#config>,
            ) -> Option<Self>
            {
                #variant_checks
                None
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::#ast_type_name<#config> {
                match self {
                    #unwrap_variants
                }
            }
        }
    )
    .into())
}

fn ast_macro_struct(
    input: &syn::DeriveInput,
    data: &syn::DataStruct,
    cfg: MacroConfig,
) -> Result<TokenStream2> {
    let MacroConfig {
        trait_name,
        ast_type_name,
        assoc_type_name,
    } = cfg;
    let ident = &input.ident;

    if !data.fields.is_empty() {
        return Err(error("`ast_node` struct cannot have any fields"));
    }
    let config = parse_config_ident();

    let struct_decl = {
        let vis = &input.vis;
        quote! {
            #[derive(Copy, Clone, Eq, PartialEq, Hash)]
            #vis struct #ident(::eventree_wrapper::eventree::#ast_type_name<#config>);
        }
    };

    Ok(quote!(
        #struct_decl

        impl ::eventree_wrapper::syntax_tree::#trait_name<#config> for #ident {
            fn cast(
                x: ::eventree_wrapper::eventree::#ast_type_name<#config>,
                tree: &::eventree_wrapper::eventree::SyntaxTree<#config>,
            ) -> Option<Self>
            {
                if x.kind(tree) == <#config as ::eventree_wrapper::eventree::TreeConfig>::#assoc_type_name::#ident {
                    Some(Self(x))
                } else {
                    None
                }
            }

            fn syntax(self) -> ::eventree_wrapper::eventree::#ast_type_name<#config> {
                self.0
            }
        }
    ).into())
}

fn error(message: &str) -> syn::Error {
    syn::Error::new(Span::call_site(), message)
}

/// Sets the default parse configuration to the given type in the current module.
///
/// The type must implement the `ParseConfig` trait.
///
/// # Examples
/// ```ignore
/// # use eventree_wrapper_derive as eventree_wrapper;
/// eventree_wrapper::parse_config!(crate::path::to::MyParseConfig);
/// ```
#[proc_macro]
pub fn parse_config(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::Type);
    let input_span = input.span();

    let assert_implements_parse_config = quote::quote_spanned! {input_span=>
        const _: () = {
            struct AssertImplementsParseConfig where
                #input: ::eventree_wrapper::parser::ParseConfig;
        };
    };
    let type_alias_name = parse_config_ident();
    quote!(
        type #type_alias_name = #input;
        #assert_implements_parse_config
    )
    .into()
}
