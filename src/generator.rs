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
            generate_stub_for_duplicate_struct_with_value_initialised(schtruct, fields, exprs, 2)
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
    let duplicated_name = expr.ident().unwrap();
    let initializer = if let MetaParam::ListTokens(lt) = expr {
        crate::fragment::find_value_in_token(lt.clone(), "initializer")
    } else {
        todo!("not a list token eh?")
    };

    TokenStream::from(quote! {
        mod #mod_name {
            use super::*;

            #(#attributes)*
            struct #name{
                #(#members),*
            }

            pub fn foo() {
                let f = <#duplicated_name>::#initializer;
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
    expr: &Hasheimer<u8, MetaParam>,
    offset: u8,
) -> TokenStream {
    let value = expr.get(&2).unwrap().first().unwrap().ident();

    let field = fields.get(&offset);

    let Structure {
        name,
        attributes,
        members,
        ..
    } = schtruct.get_parts(fields);
    let initialiser = quote!(<#name>::default());

    let duplicated_name = expr.get(&0).unwrap().first().unwrap().ident();

    let rhs = match expr.get(&2) {
        Some(OneOrMany::Single(mp)) => vec![mp.ident().unwrap()],
        Some(OneOrMany::Many(es)) => es.into_iter().filter_map(|expr| expr.ident()).collect(),
        _ => todo!("neither one nor many"),
    };

    //

    let (fs, es) = match (fields.get(&2), expr.get(&2)) {
        (Some(OneOrMany::Single(v)), Some(OneOrMany::Single(MetaParam::NameValueExpr(e)))) => {
            if let FilteredMember::Field(f) = v {
                (vec![f.ident.clone()], vec![ParseableExpr(e.clone())])
            } else {
                todo!()
            }
        }
        _ => todo!(),
    };

    TokenStream::from(quote! {
        // #[cfg(test)]
        mod tests {
            use super::*;

            #(#attributes)*
            struct #name{
                #(#members),*
            }

            #[test]
            pub fn foo() {
                let ob = FooMock::mock_new();
                if (#(ob.#fs),*) != (#(#es),*) {
                    panic!("get out of here");
                }

                // assert_eq!((#(ob.#fs),*), (#(#es),*));
            }
        }
    })
}
