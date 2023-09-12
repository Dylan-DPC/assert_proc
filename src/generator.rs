use crate::fragment::{Disintegrate, FilteredField, FilteredMember};
use hasheimer::{oom::OneOrMany, Hasheimer};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::collections::HashMap;
use syn::{Expr, ExprPath, Ident, Item, ItemEnum, ItemStruct};

pub fn generate_tokens(
    sigma: u8,
    item: &Item,
    exprs: &Hasheimer<u8, Expr>,
    fields: &HashMap<u8, FilteredMember>,
) -> TokenStream {
    match (sigma, item) {
        (1, Item::Struct(schtruct)) => {
            generate_stub_for_duplicate_struct(schtruct, exprs.get_if_single(&0u8).unwrap())
        }
        (1, Item::Enum(enoom)) => {
            generate_stub_for_duplicate_enum(enoom, exprs.get_if_single(&0).unwrap())
        }
        (2, Item::Struct(schtruct)) => {
            generate_stub_for_type_change(schtruct, exprs.get(&1).unwrap(), fields.get(&1).unwrap())
        }
        _ => todo!("silly"),
    }
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_duplicate_struct(schtruct: &ItemStruct, expr: &Expr) -> TokenStream {
    let duplicated_name = crate::fragment::param_from_expr(expr);
    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
    let (struct_name, attributes, fields) = schtruct.get_parts();
    let mod_name = Ident::new(format!("tests_{struct_name}").as_str(), Span::call_site());
    TokenStream::from(quote! {
        mod #mod_name {
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
    let duplicated_name = crate::fragment::param_from_expr(expr);
    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
    let (enum_name, attributes, variants) = enoom.get_parts();
    let mod_name = Ident::new(format!("tests_{enum_name}").as_str(), Span::call_site());
    TokenStream::from(quote! {
        mod #mod_name {
            use super::*;
            #(#attributes)*
            enum #enum_name {
                #(#variants),*
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
    expr: &OneOrMany<Expr>,
    field: &FilteredMember,
) -> TokenStream {
    // TODO: adjust this for vector case
    let nft = match &expr[0] {
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident(),
        _ => unreachable!(),
    };

    let (struct_name, attributes, fields) = schtruct.get_parts();
    let initialiser = quote!(<#struct_name>::default());
    let f = if let FilteredMember::Field(FilteredField { ident: Some(f), .. }) = field {
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
