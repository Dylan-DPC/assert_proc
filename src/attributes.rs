use core::iter::Extend;
use proc_macro::TokenStream;
use syn::Meta;
use syn::{Fields, Item};

pub const TYPE_OPTIONS: [&str; 2] = ["assert_duplicated", "assert_field_type"];

pub fn prepare_tokens(item: &Item) -> TokenStream {
    let (sigma, params, fields) = match item {
        Item::Struct(schtruct) => {
            let (sigma, params) = schtruct
                .attrs
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
                .fold((0, vec![]), |(mut sigma, mut params), (mv, attr)| {
                    sigma += 1 << attr.unwrap();
                    params.push(mv.value.clone());
                    (sigma, params)
                });

            let (sigma, params, fields) = match schtruct.fields {
                Fields::Named(ref named) => {
             named.named.iter().fold((sigma, params, Vec::<&syn::Field>::new()), |(mut sigma, mut params, mut fields), ref field| {
                let (sigma_f, params_f, fields_f) = field.attrs.iter().fold((0, vec![], vec![]), |(mut sigma_f, mut params_f, mut fields_f), attr| {
                    if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x == mv.path.segments.first().unwrap().ident.to_string().as_str()) {
                        sigma_f += 1 << attrib;
                        params_f.push(mv.value.clone());
                        fields_f.push(field);
                    }

                    (sigma_f, params_f, fields_f)
                });

                sigma += sigma_f;
                params.extend(params_f);
                fields.extend(fields_f);
                (sigma, params, fields)
            })

                },
                _ => todo!(),
            };
            (sigma, params, fields)
        }
        Item::Enum(enoom) => {
            todo!()
        }
        _ => todo!(),
    };

    crate::generator::generate_tokens(sigma, item, &params, &fields)
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
