use crate::fragment::{Disintegrate, FilteredField, FilteredMember};
use hasheimer::{oom::OneOrMany, Hasheimer};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Expr, ExprPath, Ident, Item, ItemEnum, ItemStruct};

pub fn generate_tokens(
    sigma: u8,
    item: &Item,
    exprs: &Hasheimer<u8, Expr>,
    fields: &Hasheimer<u8, FilteredMember>,
) -> TokenStream {
    match (sigma, item) {
        (1, Item::Struct(schtruct)) => {
            generate_stub_for_duplicate_struct(schtruct, fields, exprs.get_if_single(&0u8).unwrap())
        }
        (1, Item::Enum(enoom)) => {
            generate_stub_for_duplicate_enum(enoom, fields, exprs.get_if_single(&0).unwrap())
        }
        (2, Item::Struct(schtruct)) => {
            generate_stub_for_type_change(schtruct, fields, exprs.get(&1).unwrap(), 1)
        }
        (2, Item::Enum(enoom)) => {
            todo!()
            // generate_stub_for_enum_data_change(enoom, fields, exprs.get(&1).unwrap(), fields.get(&1).unwrap())
        }
        _ => todo!("silly"),
    }
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_duplicate_struct(
    schtruct: &ItemStruct,
    fields: &Hasheimer<u8, FilteredMember>,
    expr: &Expr,
) -> TokenStream {
    let duplicated_name = crate::fragment::param_from_expr(expr);
    let (struct_name, attributes, fields) = schtruct.get_parts(fields);
    assert_ne!(
        struct_name, duplicated_name,
        "Duplicated struct name is same as original name"
    );
    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
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

pub fn generate_stub_for_duplicate_enum(
    enoom: &ItemEnum,
    fields: &Hasheimer<u8, FilteredMember>,
    expr: &Expr,
) -> TokenStream {
    let duplicated_name = crate::fragment::param_from_expr(expr);
    let duplicated_name = Ident::new(format!("{duplicated_name}").as_str(), Span::call_site());
    let initialiser = quote!(<#duplicated_name>::default());
    let (enum_name, attributes, variants) = enoom.get_parts(fields);
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
    fields: &Hasheimer<u8, FilteredMember>,
    expr: &OneOrMany<Expr>,
    offset: u8,
) -> TokenStream {
    // TODO: adjust this for vector case
    let nft = match &expr[0] {
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident(),
        _ => unreachable!(),
    };

    let field = fields.get(&offset);

    let (struct_name, attributes, struct_fields) = schtruct.get_parts(fields);
    let initialiser = quote!(<#struct_name>::default());
    let f = if let Some(OneOrMany::Single(FilteredMember::Field(FilteredField {
        ident: Some(f),
        ..
    }))) = field
    {
        Ident::new(format!("{f}").as_str(), Span::call_site())
    } else {
        todo!()
    };
    TokenStream::from(quote! {
        mod tests {
            use super::*;

            #(#attributes)*
            struct #struct_name {
                #(#struct_fields),*
            }

            pub fn foo() {
                let ob = #initialiser;
                let f: #nft = ob.#f;
            }
        }
    })
}

/*
pub fn generate_stub_for_enum_data_change(enoom: &ItemEnum, fields: HashMap<u8, FilteredMember>, expr: &OneOrMany<Expr>, field: &FilteredMember) -> TokenStream {
    let nft = match &expr[0] {
        Expr::Path(ExprPath { path: p, ..}) => p.get_ident(),
        _ => unreachable!(),
    };

    let (enum_name, attributes, fields) = enoom.get_parts(fields);
    let initialiser = quote!(<#enum_name::default());
    let variant = if let FilteredMember::VariantData(variant, FilteredField {.. }) = field {
        variant
    } else {
        todo!()
    };


    let mod_name = Ident::new(format!("tests_{enum_name}").as_str(), Span::call_site());
    let fields = fields.values();
    TokenStream::from(quote! {
            mod #mod_name {
                use super::*;

               // #(#attributes)*
                struct #enum_name{
                     #(#fields),*
                }

                pub fn foo() {
                    // let ob = #initialiser;
                    // let f: #nft = match ob {
                        //#variant(f) => f
                    // }
                }
            }
    })

}
*/
