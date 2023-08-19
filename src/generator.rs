use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Expr, ExprLit, Field, Fields, ItemStruct, Lit, Meta};

pub fn generate_tokens(
    sigma: u8,
    schtruct: &ItemStruct,
    exprs: &[&Expr],
    fields: &[&Field],
) -> TokenStream {
    match sigma {
        1 => generate_stub_for_duplicate(schtruct, exprs[0]),
        2 => generate_stub_for_type_change(schtruct, exprs[0], fields[0]),
        _ => todo!(),
    }
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_duplicate(schtruct: &ItemStruct, expr: &Expr) -> TokenStream {
    let duplicated_name = match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(l), ..
        }) => l,
        _ => unreachable!(),
    };

    let duplicated_name: syn::Type = syn::parse_str(duplicated_name.value().as_str()).unwrap();
    let initialiser = quote!(<#duplicated_name>::default());
    let (struct_name, attributes, fields) = get_struct_parts(schtruct);

    TokenStream::from(quote! {
        mod tests {
            use super::*;

            #(#attributes)*
            struct #struct_name {
                #fields
            }

            pub fn foo() {
                let f = #initialiser;
            }
        }
    })
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_type_change(
    schtruct: &ItemStruct,
    expr: &Expr,
    field: &Field,
) -> TokenStream {
    let nft = match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(l), ..
        }) => l,
        _ => unreachable!(),
    };

    let (struct_name, attributes, fields) = get_struct_parts(schtruct);
    let initialiser = quote!(<#struct_name>::default());
    let (struct_name, attributes, fields) = get_struct_parts(schtruct);
    TokenStream::from(quote! {
        mod tests {
            use super::*;

            #(#attributes)*
            struct #struct_name {
                #fields
            }

            pub fn foo() {
                let ob = #initialiser;
                // let _f : #struct_name = ob.<#field>;
            }
        }
    })
}

fn get_struct_parts(
    schtruct: &ItemStruct,
) -> (
    &syn::Ident,
    impl Iterator<Item = Attribute> + '_,
    &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) {
    let struct_name = &schtruct.ident;

    let attributes = schtruct.attrs.iter().filter_map(|attr| {
        match attr.meta {
            Meta::Path(ref p) if let Some(path) = p.get_ident() && path.to_string().starts_with("assert_") => {
                Option::<Attribute>::None
            },

            Meta::NameValue(ref nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => {
                None
            },

            _ => {
                Some(syn::parse_quote!(#attr))
            },

        }
    });

    let fields = match schtruct.fields {
        Fields::Named(ref f) => &f.named,
        Fields::Unnamed(ref f) => &f.unnamed,
        Fields::Unit => todo!(),
    };

    (struct_name, attributes, fields)
}
