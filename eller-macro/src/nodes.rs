use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced,
    parse::{Lookahead1, Parse, ParseStream},
    token::Brace,
    Expr, Ident, Path, Token, Type,
};

#[derive(Debug)]
pub struct Element {
    open_tag: OpenTag,
    children: Vec<Node>,
    close_tag: Option<CloseTag>,
}

impl Parse for Element {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let open_tag = input.parse::<OpenTag>()?;

        let (children, close_tag) = if open_tag.is_self_closing() {
            (Vec::new(), None)
        } else {
            let children = Node::parse_list(input)?;
            let close_tag = input.parse::<CloseTag>()?;

            if open_tag.name != close_tag.name {
                return Err(syn::Error::new_spanned(
                    &close_tag.name,
                    format!(
                        "Tag <{}/> does not match tag <{}>",
                        close_tag.name.to_token_stream(),
                        open_tag.name.to_token_stream(),
                    ),
                ));
            }

            (children, Some(close_tag))
        };

        Ok(Element {
            open_tag,
            children,
            close_tag,
        })
    }
}

impl Element {
    pub fn generate(
        &self,
        prefix: Option<&String>,
        element_trait: Option<&Path>,
    ) -> syn::Result<TokenStream> {
        let mut tokens = TokenStream::new();

        let element_name = &self.open_tag.name;

        let builder = if let Some(prefix) = prefix {
            format_ident!("{}_builder", prefix)
        } else {
            format_ident!("builder")
        };

        let build = if let Some(prefix) = prefix {
            format_ident!("{}_build", prefix)
        } else {
            format_ident!("build")
        };

        let add_child = if let Some(prefix) = prefix {
            format_ident!("{}_child", prefix)
        } else {
            format_ident!("child")
        };

        if let Some(element_trait) = &element_trait {
            tokens.extend(quote! {
                <#element_name as #element_trait>::#builder()
            });
        } else {
            tokens.extend(quote! {
                <#element_name>::#builder()
            });
        }

        for attr in &self.open_tag.attributes {
            let set_attr = if let Some(prefix) = prefix {
                format_ident!("{}_attr_{}", prefix, attr.name)
            } else {
                format_ident!("attr_{}", attr.name)
            };

            let value = &attr.value;

            tokens.extend(quote! {
                .#set_attr(#value)
            })
        }

        for child in &self.children {
            let child_tokens = child.generate(prefix, element_trait)?;
            tokens.extend(quote! {
                .#add_child(#child_tokens)
            })
        }

        tokens.extend(quote! {
            .#build()
        });

        Ok(tokens)
    }
}

#[derive(Debug)]
pub enum Node {
    Element(Element),
    Expr(Brace, Expr),
}

impl Node {
    fn parse_opt(input: ParseStream) -> syn::Result<Option<Node>> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![<]) {
            if input.peek2(Token![/]) {
                Ok(None)
            } else {
                input.parse().map(Node::Element).map(Some)
            }
        } else if lookahead.peek(Brace) {
            let content;
            let brace = braced!(content in input);
            let expr = content.parse()?;
            Ok(Some(Node::Expr(brace, expr)))
        } else {
            Err(lookahead.error())
        }
    }

    pub fn parse_list(input: ParseStream) -> syn::Result<Vec<Node>> {
        let mut nodes = Vec::new();

        while let Some(node) = Node::parse_opt(input)? {
            nodes.push(node)
        }

        Ok(nodes)
    }

    pub fn generate(
        &self,
        prefix: Option<&String>,
        element_trait: Option<&Path>,
    ) -> syn::Result<TokenStream> {
        match self {
            Node::Element(element) => element.generate(prefix, element_trait),
            Node::Expr(_, expr) => Ok(expr.to_token_stream()),
        }
    }
}

#[derive(Debug)]
pub struct OpenTag {
    lt_token: Token![<],
    name: Type,
    attributes: Vec<TagAttribute>,
    close_token: Option<Token![/]>,
    gt_token: Token![>],
}

impl OpenTag {
    pub fn is_self_closing(&self) -> bool {
        self.close_token.is_some()
    }
}

impl Parse for OpenTag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lt_token = input.parse()?;
        let name = input.parse()?;

        let mut attributes = Vec::new();
        let close_token;
        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![/]) {
                close_token = Some(input.parse()?);
                break;
            } else if lookahead.peek(Token![>]) {
                close_token = None;
                break;
            } else if TagAttribute::peek(&lookahead) {
                attributes.push(input.parse()?);
            } else {
                return Err(lookahead.error());
            }
        }

        let gt_token = input.parse()?;

        Ok(OpenTag {
            lt_token,
            name,
            attributes,
            close_token,
            gt_token,
        })
    }
}

#[derive(Debug)]
pub struct TagAttribute {
    name: Ident,
    eq_token: Token![=],
    brace: Brace,
    value: Expr,
}

impl Parse for TagAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(TagAttribute {
            name: input.parse()?,
            eq_token: input.parse()?,
            brace: braced!(content in input),
            value: content.parse()?,
        })
    }
}

impl TagAttribute {
    pub fn peek(input: &Lookahead1<'_>) -> bool {
        input.peek(Ident)
    }
}

#[derive(Debug)]
pub struct CloseTag {
    lt_token: Token![<],
    close_token: Token![/],
    name: Type,
    gt_token: Token![>],
}

impl Parse for CloseTag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(CloseTag {
            lt_token: input.parse()?,
            close_token: input.parse()?,
            name: input.parse()?,
            gt_token: input.parse()?,
        })
    }
}
