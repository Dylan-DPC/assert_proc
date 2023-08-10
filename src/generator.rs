use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Expr, ExprLit, Fields, ItemStruct, Lit, Meta};

#[allow(clippy::manual_let_else)]
pub fn generate_test_stub_for_duplicate(schtruct: &ItemStruct, expr: &Expr) -> TokenStream {
    let duplicated_name = match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(l), ..
        }) => l,
        _ => unreachable!(),
    };

    let duplicated_name: syn::Type = syn::parse_str(duplicated_name.value().as_str()).unwrap();
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
    let initialiser = quote!(<#duplicated_name>::default());
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
