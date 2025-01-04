//! Flexible and easy abstraction over the multistate typestate pattern
//!
//! Provides an easy and flexible way to apply an extended version of the typestate pattern to
//! structs.
//!
//! # Example
//! ```
//! use std::marker::PhantomData;
//! use typelist::typelist;
//!
//! // Create possible states
//! struct FooState;
//! struct BarState;
//!
//! // Allow typelist to work its magic
//! typelist!(2, FooState, BarState);
//!
//! // Struct using the typestate
//! struct Node<S = Nil> {
//!   _state: PhantomData<S>,
//! }
//!
//! impl<S> Node<S> {
//!   pub fn new() -> Self {
//!     Node { _state: PhantomData }
//!   }
//!
//!   // Function adding the FooState state
//!   pub fn foo(&self) -> Node<Cons<FooState, S>> {
//!     Node::new()
//!   }
//!
//!   // Function adding the BarState state
//!   pub fn bar(&self) -> Node<Cons<BarState, S>> {
//!     Node::new()
//!   }
//! }
//!
//! // This implementation can only be used if FooState has been added
//! impl<S> Node<S> where S: Includes<FooState> {
//!   pub fn only_on_foo(&self) { }
//! }
//!
//! // This implementation can only be used if BarState has been added
//! impl<S> Node<S> where S: Includes<BarState> {
//!   pub fn only_on_bar(&self) { }
//! }
//!
//! // This implementation can only be used if both FooState and BarState has been added
//! impl<S> Node<S> where S: Includes<BarState> + Includes<FooState> {
//!   pub fn only_on_foo_and_bar(&self) { }
//! }
//!
//! let node = Node::new();
//!
//! // Allowed
//! node.foo().only_on_foo();
//! node.bar().foo().only_on_foo();
//! node.foo().bar().only_on_foo();
//!
//! node.foo().bar().only_on_foo_and_bar();
//! node.bar().foo().only_on_foo_and_bar();
//!
//! // Not Allowed
//! // node.bar().only_on_foo();
//! // node.foo().only_on_foo_and_bar();
//! // node.bar().only_on_foo_and_bar();
//! ```

use itertools::Itertools;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, parse_str, Expr, Ident, Token};

struct MacroInput {
    depth: Expr,
    names: Punctuated<Ident, Token![,]>,
}

struct ConvertedInput {
    depth: i32,
    names: Vec<String>,
}

struct Options {
    generate_structs: bool,
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let depth: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let names = Punctuated::parse_terminated(input)?;
        Ok(Self { depth, names })
    }
}

impl From<MacroInput> for ConvertedInput {
    fn from(input: MacroInput) -> Self {
        let depth = match input.depth {
            Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Int(lit),
                ..
            }) => lit.base10_parse::<i32>().expect("Invalid integer literal"),
            _ => panic!("Expected a literal integer as the first argument"),
        };
        let names = input
            .names
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        Self { depth, names }
    }
}

fn generate_structs(names: &[String]) -> proc_macro2::TokenStream {
    let structs: Vec<_> = names
        .iter()
        .map(|name| {
            let ident: Ident = parse_str(name).expect("{name} is not a valid identifier");
            quote! { pub struct #ident; }
        })
        .collect();
    quote! { #(#structs)* }
}

fn generate_impls(depth: i32, names: &[String]) -> proc_macro2::TokenStream {
    let impls: Vec<_> = (1..=depth)
        .flat_map(|d| {
            names
                .iter()
                .flat_map(|name| {
                    // Create an iterator over all impls containing the given name
                    let product = (0..d).map(|_| names.iter()).multi_cartesian_product();
                    product
                        .map(|x| (x.contains(&name), x))
                        // Map each permutation to its impl
                        .map(|(is_included, name_permutation)| {
                            let name_ident: Ident =
                                parse_str(name).expect("{name} is not a valid identifier");
                            let trait_ident = if is_included {
                                quote! { Includes }
                            } else {
                                quote! { Excludes }
                            };
                            let list = name_permutation.iter().map(|inner_name| {
                                let ident: Ident = parse_str(inner_name)
                                    .expect("{name} is not a valid identifier");
                                quote! { Cons<#ident,  }
                            });
                            let ending_gts = (0..d).map(|_| quote! {>}).collect::<Vec<_>>();
                            quote! {
                                impl #trait_ident<#name_ident> for #(#list)* Nil #(#ending_gts)* {}
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect();
    quote! { #(#impls)* }
}

/**
 * # Panics
 * may panic if given names which cannot be identifiers
 * */
fn generate_output(
    ConvertedInput { depth, names }: ConvertedInput,
    options: &Options,
) -> proc_macro2::TokenStream {
    let impls = generate_impls(depth, &names);
    let structs = generate_structs(&names);

    let generated = if options.generate_structs {
        quote! {
            #structs
            #impls
        }
    } else {
        quote! { #impls }
    };

    let output = quote! {
        pub trait Includes<T> {}
        pub trait Excludes<T> {}
        pub struct Nil;
        pub struct Cons<H, T>(PhantomData<(H, T)>);
        impl Includes<Nil> for Nil {}
        impl<H, T> Includes<Nil> for Cons<H, T> {}
        impl<T> Excludes<T> for Nil {}
        #generated
    };

    output
}

#[proc_macro]
/// # Panics
/// may panic if given names which cannot be identifiers
pub fn typelist(input: TokenStream) -> TokenStream {
    let converted_input = parse_macro_input!(input as MacroInput).into();
    generate_output(
        converted_input,
        &Options {
            generate_structs: false,
        },
    )
    .into()
}

#[proc_macro]
/// # Panics
/// may panic if given names which cannot be identifiers
pub fn typelist_with_structs(input: TokenStream) -> TokenStream {
    let converted_input = parse_macro_input!(input as MacroInput).into();
    generate_output(
        converted_input,
        &Options {
            generate_structs: true,
        },
    )
    .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_tokenstreams_eq::assert_tokenstreams_eq;

    #[test]
    fn struct_token_streams_match() {
        let names: Vec<String> = vec!["Foo".into(), "Bar".into()];
        let generated = generate_structs(&names);
        let expected = quote! {
            pub struct Foo;
            pub struct Bar;
        };
        assert_tokenstreams_eq!(&generated, &expected);
    }

    #[test]
    fn impl_token_streams_match() {
        let names: Vec<String> = vec!["Foo".into(), "Bar".into()];
        let generated = generate_impls(2, &names);
        let expected = quote! {
            impl Includes<Foo> for Cons<Foo, Nil> {}
            impl Excludes<Foo> for Cons<Bar, Nil> {}
            impl Excludes<Bar> for Cons<Foo, Nil> {}
            impl Includes<Bar> for Cons<Bar, Nil> {}
            impl Includes<Foo> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Excludes<Foo> for Cons<Bar, Cons<Bar, Nil>> {}
            impl Excludes<Bar> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Bar, Nil>> {}
        };
        assert_tokenstreams_eq!(&generated, &expected);
    }

    #[test]
    fn full_token_streams_match() {
        let input = ConvertedInput {
            depth: 2,
            names: vec!["Foo".into(), "Bar".into()],
        };
        let options = Options {
            generate_structs: true,
        };
        let generated = generate_output(input, &options);
        let expected = quote! {
            pub trait Includes<T> {}
            pub trait Excludes<T> {}
            pub struct Nil;
            pub struct Cons<H, T>(PhantomData<(H, T)>);
            impl Includes<Nil> for Nil {}
            impl<H, T> Includes<Nil> for Cons<H, T> {}
            impl<T> Excludes<T> for Nil {}
            pub struct Foo;
            pub struct Bar;
            impl Includes<Foo> for Cons<Foo, Nil> {}
            impl Excludes<Foo> for Cons<Bar, Nil> {}
            impl Excludes<Bar> for Cons<Foo, Nil> {}
            impl Includes<Bar> for Cons<Bar, Nil> {}
            impl Includes<Foo> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Excludes<Foo> for Cons<Bar, Cons<Bar, Nil>> {}
            impl Excludes<Bar> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Bar, Nil>> {}
        };
        assert_tokenstreams_eq!(&generated, &expected);
    }

    #[test]
    fn impl_depth_3_token_streams_match3() {
        let names = vec!["Foo".into(), "Bar".into()];
        let generated = generate_impls(3, &names);
        let expected = quote! {
            impl Includes<Foo> for Cons<Foo, Nil> {}
            impl Excludes<Foo> for Cons<Bar, Nil> {}
            impl Excludes<Bar> for Cons<Foo, Nil> {}
            impl Includes<Bar> for Cons<Bar, Nil> {}
            impl Includes<Foo> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Excludes<Foo> for Cons<Bar, Cons<Bar, Nil>> {}
            impl Excludes<Bar> for Cons<Foo, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Bar, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Foo, Nil>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Bar, Nil>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Foo, Cons<Foo, Nil>>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Foo, Cons<Bar, Nil>>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Bar, Cons<Foo, Nil>>> {}
            impl Includes<Foo> for Cons<Foo, Cons<Bar, Cons<Bar, Nil>>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Foo, Cons<Foo, Nil>>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Foo, Cons<Bar, Nil>>> {}
            impl Includes<Foo> for Cons<Bar, Cons<Bar, Cons<Foo, Nil>>> {}
            impl Excludes<Foo> for Cons<Bar, Cons<Bar, Cons<Bar, Nil>>> {}
            impl Excludes<Bar> for Cons<Foo, Cons<Foo, Cons<Foo, Nil>>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Foo, Cons<Bar, Nil>>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Bar, Cons<Foo, Nil>>> {}
            impl Includes<Bar> for Cons<Foo, Cons<Bar, Cons<Bar, Nil>>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Foo, Cons<Foo, Nil>>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Foo, Cons<Bar, Nil>>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Bar, Cons<Foo, Nil>>> {}
            impl Includes<Bar> for Cons<Bar, Cons<Bar, Cons<Bar, Nil>>> {}
        };
        assert_tokenstreams_eq!(&generated, &expected);
    }
}
