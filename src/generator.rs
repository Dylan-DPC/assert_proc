use crate::attributes::FilteredField;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Attribute, Expr, ExprPath, Field, Fields, Ident, ItemStruct, Meta};

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
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident().unwrap(),

        _ => {
            todo!()
        }
    };

    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
    let (struct_name, attributes, fields) = get_struct_parts(schtruct);
    TokenStream::from(quote! {
        mod tests {
            use super::*;

            #(#attributes)*
            struct #struct_name {
                #(#fields),*
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
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident(),
        _ => unreachable!(),
    };

    let (struct_name, attributes, fields) = get_struct_parts(schtruct);
    let initialiser = quote!(<#struct_name>::default());
    let f = if let Field { ident: Some(f), .. } = field {
        Ident::new(format!("{f}").as_str(), Span::call_site())
    } else {
        todo!()
    };

    TokenStream::from(quote! {
        mod tests {
            use super::*;

            #(#attributes)*
            struct #struct_name {
                #(#fields),*
            }

            pub fn foo() {
                let ob = #initialiser;
                let f: #nft = ob.#f;
            }
        }
    })
}

fn get_struct_parts(
    schtruct: &ItemStruct,
) -> (
    &Ident,
    impl Iterator<Item = Attribute> + '_,
    impl Iterator<Item = FilteredField> + '_,
) {
    let struct_name = &schtruct.ident;

    let attributes = schtruct.attrs.iter().filter_map(|attr| {
        match &attr.meta {
            Meta::Path(p) if let Some(path) = p.get_ident() && path.to_string().starts_with("assert_") => {
                None
            },

            Meta::Path(p) if let Some(path) = p.get_ident() => {
                Some(syn::parse_quote!(#attr))
            },

            Meta::List(ml) if let Some(path) = ml.path.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::List(ml) if let Some(path) = ml.path.get_ident() => {
                Some(syn::parse_quote!(#attr))
            },

            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() => {
                Some(syn::parse_quote!(#attr))
            }

            _ => unreachable!()
        }
    });

    let fields = match &schtruct.fields {
        Fields::Named(f) => &f.named,
        Fields::Unnamed(f) => &f.unnamed,
        Fields::Unit => todo!(),
    };

    let fields = fields.iter().map(|field| {
        let attr = field.attrs.iter().filter_map(|attr| {
        match &attr.meta {
            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::Path(p) if let Some(path) = p.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::Path(p) if let Some(p) = p.get_ident() => {
                Some(attr.clone())
            }

            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() => {
                Some(attr.clone())
            },


        _ => {
            todo!()
        }
        }
        });

        FilteredField::new(field.clone(), attr.collect())
    });

    (struct_name, attributes, fields)
}
