use crate::generator::TokenGenerator;
use crate::proxy::{FilteredField, FilteredMember, FilteredVariant, MetaParam};
use core::iter::Extend;
use hasheimer::Hasheimer;
use proc_macro::TokenStream;
use syn::{Attribute, Item, Meta};

pub const TYPE_OPTIONS: [&str; 3] = ["assert_duplicated", "assert_field_type", "assert_value"];

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
            .map(|field| {
                FilteredMember::Field(FilteredField::new(field.clone(), field.attrs.clone()))
            })
            .collect(),

        Item::Enum(e) => e
            .variants
            .iter()
            .map(|variant| {
                let fv = FilteredVariant::new(variant.clone(), variant.attrs.clone());

                if variant.fields.is_empty() {
                    FilteredMember::Variant(fv)
                } else {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| FilteredField::new(field.clone(), field.attrs.clone()))
                        .collect();
                    FilteredMember::VariantData(fv, fields)
                }
            })
            .collect(),

        _ => todo!(),
    };

    let (sigma_f, params_f, members_f) = reduce_member_attributes(&members);
    sigma += sigma_f;
    params.extend(params_f);

    let generator = TokenGenerator::new(sigma, item, &params, &members_f);
    TokenStream::from(generator.generate())
}

#[allow(clippy::cast_possible_truncation)]
fn reduce_type_attributes(attrs: &[Attribute]) -> (u8, Hasheimer<u8, MetaParam>) {
    attrs
        .iter()
        .filter_map(|attr| {
            match &attr.meta {
                Meta::List(ml) if let Some(assert_pos) = TYPE_OPTIONS.iter().position(|x| *x == ml.path.segments.first().unwrap().ident.to_string().as_str()) => {
                    Some((
                        assert_pos,
                        MetaParam::ListTokens(ml.tokens.clone())
                    ))
                },
                Meta::NameValue(nv) if let Some(assert_pos) = TYPE_OPTIONS.iter().position(|x| *x == nv.path.segments.first().unwrap().ident.to_string().as_str()) => {
                    Some((
                         assert_pos,
                        MetaParam::NameValueExpr(nv.value.clone())
                    ))
                },
                Meta::Path(p) if let Some(assert_pos) = TYPE_OPTIONS.iter().position(|x| p.is_ident(x)) => {
                    Some((
                        assert_pos,
                        MetaParam::Path,
                    ))
                },

                _ => None
            }
        })
        .fold(
            (0, Hasheimer::default()),
            |(mut sigma, mut params), (attr, meta_param)| {
                let index = attr as u8;
                sigma += 1 << index;
                params.insert(index, meta_param);
                (sigma, params)
            },
        )
}

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cmp_owned)]
fn reduce_member_attributes(
    field_attrs: &[FilteredMember],
) -> (u8, Hasheimer<u8, MetaParam>, Hasheimer<u8, FilteredMember>) {
    let (sigma, params, members, _) = field_attrs.iter().fold((0, Hasheimer::default(), Hasheimer::default(), false), |(sigma, params, mut members, mut scanned), member | {

        let (attributes, variant) = match member {
            FilteredMember::Field(field) => (field.attributes.iter(), None),
            FilteredMember::Variant(variant) => (variant.attributes.iter(), None),
            FilteredMember::VariantData(variant, fields) => (variant.attributes.iter(), Some((variant, fields))),
        };
        let (mut sigma_f, mut params_f, mut members_f) = if attributes.is_empty() {
            members.insert(255u8, member.clone());
            scanned = true;
            (sigma, params, members)
        } else {
        attributes.fold((sigma, params, members), |(mut sigma_f, mut params_f, mut members_f), attr| {
            if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x == mv.path.segments.first().unwrap().ident.to_string()) {
            let index = attrib as u8;
            sigma_f+= 1 << index;
            params_f.insert(index, MetaParam::NameValueExpr(mv.value.clone()));
            members_f.insert(index, member.clone());
            scanned = true;
            }


            (sigma_f, params_f, members_f)
        })
        };
        if let Some((variant, fields)) = variant  {
        let (sigma_g, params_g, members_g) = (*fields).iter().fold((0, Hasheimer::<u8, MetaParam>::default(), Hasheimer::<u8, FilteredMember>::default()), |(mut sigma_g, mut params_g, mut members_g), field| {
            let (sigma_h, params_h, members_h) = field.attributes.iter().fold((0, Hasheimer::default(), Hasheimer::<u8, FilteredMember>::default()), |(mut sigma_h, mut params_h, mut members_h), attr| {
            if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x == mv.path.segments.first().unwrap().ident.to_string()) {
                let index = attrib as u8;
                sigma_h += 1 << index;
                params_h.insert(index, MetaParam::NameValueExpr(mv.value.clone()));
                members_h.insert(index, FilteredMember::VariantData((*variant).clone(), fields.clone()));
                scanned = true;
            }
            (sigma_h, params_h, members_h)
            });

            sigma_g += sigma_h;
            params_g.extend(params_h);
            members_g.extend(members_h);
            (sigma_g, params_g, members_g)
        });
        sigma_f += sigma_g;
        params_f.extend(params_g);
        members_f.extend(members_g);
        }

       if !scanned {
           members_f.insert(255, member.clone());
       }

        (sigma_f, params_f, members_f, false)
    });
    (sigma, params, members)
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
