use crate::fragment::{FilteredField, FilteredMember};
use core::iter::Extend;
use hasheimer::Hasheimer;
use proc_macro::TokenStream;
use std::collections::HashMap;
use syn::Meta;
use syn::{Attribute, Expr, Item};

pub const TYPE_OPTIONS: [&str; 2] = ["assert_duplicated", "assert_field_type"];

pub fn prepare_tokens(item: &Item) -> TokenStream {
    let attrs = match item {
        Item::Struct(s) => &s.attrs,
        Item::Enum(e) => &e.attrs,
        _ => todo!(),
    };

    let (mut sigma, mut params) = reduce_type_attributes(attrs);

    let members: Vec<_> = match item {
        Item::Struct(s) => s
            .fields
            .iter()
            .map(|field| FilteredField::new(field.clone(), field.attrs.clone()))
            .collect(),
        _ => todo!(),
    };

    let (sigma_f, params_f, members_f) = reduce_member_attributes(&members);
    sigma += sigma_f;
    params.extend(params_f);

    crate::generator::generate_tokens(sigma, item, &params, &members_f)
}

#[allow(clippy::cast_possible_truncation)]
fn reduce_type_attributes(attrs: &[Attribute]) -> (u8, Hasheimer<u8, Expr>) {
    attrs
        .iter()
        .filter_map(|attr| {
            if let Meta::NameValue(ref mv) = attr.meta {
                Some((
                    mv,
                    TYPE_OPTIONS.iter().position(|x| {
                        *x == mv.path.segments.first().unwrap().ident.to_string().as_str()
                    }),
                ))
            } else {
                None
            }
        })
        .fold(
            (0, Hasheimer::default()),
            |(mut sigma, mut params), (mv, attr)| {
                let index = attr.unwrap() as u8;
                sigma += 1 << index;
                params.insert(index, mv.value.clone());
                (sigma, params)
            },
        )
}

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cmp_owned)]
fn reduce_member_attributes(
    field_attrs: &[FilteredField],
) -> (u8, Hasheimer<u8, Expr>, HashMap<u8, FilteredMember>) {
    field_attrs.iter().fold((0, Hasheimer::default(), HashMap::default()), |(sigma, params, members), field| {
        field.attributes.iter().fold((sigma, params, members), |(mut sigma_f, mut params_f, mut members_f), attr| {
            if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x ==mv.path.segments.first().unwrap().ident.to_string()) {
            let index = attrib as u8;
            sigma_f+= 1 << index;
            params_f.insert(index, mv.value.clone());
            members_f.insert(index, FilteredMember::Field((*field).clone()));
            }
        (sigma_f, params_f, members_f)
        })
    })
}

pub fn filter_attributes(attrs: &[Attribute]) -> impl Iterator<Item = Attribute> + '_ {
    attrs.iter().filter_map(|attr| {
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
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn foo() {
        let foo = syn::Field {
            attrs: vec![],
            vis: syn::Visibility::Inherited,
            mutability: syn::FieldMutability::None,
            ident: syn::parse_str("foo").unwrap(),
            colon_token: Some(syn::parse_str(":").unwrap()),
            ty: syn::Type::Path(syn::TypePath {
                qself: None,
                path: syn::parse_str("u32").unwrap(),
            }),
        };

        let bracket = syn::token::Bracket::default();

        let attribute = syn::Attribute {
            pound_token: syn::token::Pound {
                spans: [proc_macro2::Span::call_site()],
            },
            style: syn::AttrStyle::Outer,
            bracket_token: bracket,
            meta: syn::Meta::NameValue(syn::MetaNameValue {
                path: syn::parse_str("must_use").unwrap(),
                eq_token: syn::Token![=](proc_macro2::Span::call_site()),
                value: syn::parse_str("u32").unwrap(),
            }),
        };

        let filtered = FilteredField::new(foo, vec![attribute]);
        dbg!(quote::quote!(#filtered));
    }
}
