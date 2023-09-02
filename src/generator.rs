use crate::fragment::DisIntegrate;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Expr, ExprPath, Field, Ident, Item, ItemEnum, ItemStruct};

pub fn generate_tokens(sigma: u8, item: &Item, exprs: &[Expr], fields: &[&Field]) -> TokenStream {
    match (sigma, item) {
        (1, Item::Struct(schtruct)) => generate_stub_for_duplicate_struct(schtruct, &exprs[0]),
        (1, Item::Enum(enoom)) => generate_stub_for_duplicate_enum(enoom, &exprs[0]),
        (2, Item::Struct(schtruct)) => {
            generate_stub_for_type_change(schtruct, &exprs[0], fields[0])
        }
        _ => todo!(),
    }
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_duplicate_struct(schtruct: &ItemStruct, expr: &Expr) -> TokenStream {
    let duplicated_name = match expr {
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident().unwrap(),

        _ => {
            todo!()
        }
    };

    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
    let (struct_name, attributes, fields) = schtruct.get_parts();
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

pub fn generate_stub_for_duplicate_enum(enoom: &ItemEnum, expr: &Expr) -> TokenStream {
    todo!()
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

    let (struct_name, attributes, fields) = schtruct.get_parts();
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
