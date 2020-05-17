mod nodes;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Attribute, Lit, Meta, MetaNameValue, Path,
};

use nodes::Element;

#[proc_macro]
pub fn element(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input);

    element_macro(input)
        .map(|output| output.into_token_stream())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn element_macro(input: MacroInput) -> syn::Result<TokenStream> {
    input
        .element
        .generate(input.prefix.as_ref(), input.element_trait.as_ref())
}

#[derive(Debug)]
struct MacroInput {
    prefix: Option<String>,
    element_trait: Option<Path>,
    element: Element,
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = Attribute::parse_inner(input)?;

        let mut prefix = None;
        let mut element_trait = None;

        for attr in attrs {
            let attr = attr.parse_meta()?;
            let path = attr.path();

            if path.is_ident("prefix") {
                match attr {
                    Meta::NameValue(MetaNameValue {
                        lit: Lit::Str(value),
                        ..
                    }) => {
                        prefix = Some(value.value());
                    }

                    _ => return Err(syn::Error::new_spanned(attr, "Invalid attribute")),
                }
            } else if path.is_ident("no_prefix") {
                match attr {
                    Meta::Path(_) => prefix = None,

                    _ => return Err(syn::Error::new_spanned(attr, "Invalid attribute")),
                }
            } else if path.is_ident("element_trait") {
                match attr {
                    Meta::NameValue(MetaNameValue {
                        lit: Lit::Str(value),
                        ..
                    }) => {
                        element_trait = Some(value.parse()?);
                    }

                    _ => return Err(syn::Error::new_spanned(attr, "Invalid attribute")),
                }
            } else if path.is_ident("no_element_trait") {
                match attr {
                    Meta::Path(_) => element_trait = None,

                    _ => return Err(syn::Error::new_spanned(attr, "Invalid attribute")),
                }
            } else {
                return Err(syn::Error::new_spanned(path, "Unknown attribute"));
            }
        }

        Ok(MacroInput {
            prefix,
            element_trait,
            element: input.parse()?,
        })
    }
}
