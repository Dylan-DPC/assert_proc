use crate::fragment::Disintegrate;
use crate::proxy::{FilteredField, FilteredMember, MetaParam, ParseableExpr, Structure};
use hasheimer::{oom::OneOrMany, Hasheimer};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{Expr, ExprPath, Ident, Item, ItemEnum, ItemStruct};

pub fn generate_tokens(
    sigma: u8,
    item: &Item,
    exprs: &Hasheimer<u8, MetaParam>,
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

        (5, Item::Struct(schtruct)) => {
            generate_stub_for_duplicate_struct_with_value_initialised(schtruct, fields, exprs)
        }

        _ => todo!("silly"),
    }
}
pub fn mod_name(name: &Ident) -> Ident {
    Ident::new(format!("tests_{name}").as_str(), Span::call_site())
}

#[allow(clippy::manual_let_else)]
pub fn generate_stub_for_duplicate_struct(
    schtruct: &ItemStruct,
    fields: &Hasheimer<u8, FilteredMember>,
    expr: &MetaParam,
) -> TokenStream {
    let mod_name = mod_name(&schtruct.ident);
    let Structure {
        name,
        attributes,
        members,
    } = schtruct.get_parts(fields);
    let initialiser = match crate::fragment::get_params_for_duplicated(expr) {
        (name, Some(init)) => quote!(<#name>::#init),
        (name, _) => quote!(<#name>::default()),
    };

    TokenStream::from(quote! {
        mod #mod_name {
            use super::*;

            #(#attributes)*
            struct #name{
                #(#members),*
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
    expr: &MetaParam,
) -> TokenStream {
    let duplicated_name = expr.ident().unwrap();
    let initialiser = quote!(<#duplicated_name>::default());
    let Structure {
        name,
        attributes,
        members,
        ..
    } = enoom.get_parts(fields);
    let mod_name = Ident::new(format!("tests_{name}").as_str(), Span::call_site());
    TokenStream::from(quote! {
        mod #mod_name {
            use super::*;
            #(#attributes)*
            enum #name{
                #(#members),*
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
    expr: &OneOrMany<MetaParam>,
    offset: u8,
) -> TokenStream {
    // TODO: adjust this for vector case
    let nft = match &expr[0] {
        MetaParam::NameValueExpr(Expr::Path(ExprPath { path: p, .. })) => p.get_ident(),
        _ => unreachable!(),
    };

    let field = fields.get(&offset);

    let Structure {
        name,
        attributes,
        members,
        ..
    } = schtruct.get_parts(fields);
    let initialiser = quote!(<#name>::default());
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
            struct #name{
                #(#members),*
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

pub fn generate_stub_for_duplicate_struct_with_value_initialised(
    schtruct: &ItemStruct,
    fields: &Hasheimer<u8, FilteredMember>,
    exprs: &Hasheimer<u8, MetaParam>,
) -> TokenStream {
    let mod_name = mod_name(&schtruct.ident);
    let Structure {
        name,
        attributes,
        members,
    } = schtruct.get_parts(fields);
    let initialiser =
        match crate::fragment::get_params_for_duplicated(exprs.get_if_single(&0).unwrap()) {
            (name, Some(init)) => quote!(<#name>::#init),
            (name, _) => quote!(<#name>::default()),
        };

    let (fs, es) = match (fields.get(&2), exprs.get(&2)) {
        (
            Some(OneOrMany::Single(FilteredMember::Field(v))),
            Some(OneOrMany::Single(MetaParam::NameValueExpr(e))),
        ) => (vec![v.ident.clone()], vec![ParseableExpr(e.clone())]),
        _ => todo!(),
    };

    TokenStream::from(quote! {
        // #[cfg(test)]
        mod #mod_name {
            use super::*;

            #(#attributes)*
            struct #name{
                #(#members),*
            }



            // #[test]
            pub fn foo() {
                let ob = #initialiser;
                if (#(ob.#fs),*) != (#(#es),*) {
                   panic!("value in type doesn't match the asserted value")
                }

            }
        }
    })
}
