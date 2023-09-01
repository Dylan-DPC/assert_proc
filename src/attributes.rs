use core::iter::Extend;
use proc_macro::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::Meta;
use syn::{Attribute, Field, Fields, Ident, ItemStruct, Token, Type};

pub const TYPE_OPTIONS: [&str; 2] = ["assert_duplicated", "assert_field_type"];

pub fn prepare_tokens(schtruct: &ItemStruct) -> TokenStream {
    let (sigma, params) = schtruct.attrs.iter().fold((0, vec![]), |(mut sigma, mut params), attr| {

            if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x == mv.path.segments.first().unwrap().ident.to_string().as_str()) {
                sigma += 1 << attrib;
                params.push(&mv.value);
            };

            (sigma, params)

});

    let (sigma, params, fields) = match schtruct.fields {
        Fields::Named(ref n) => {
            n.named.iter().fold((sigma, params, Vec::<&syn::Field>::new()), |(mut sigma, mut params, mut fields), ref field| {
                let (sigma_f, params_f, fields_f) = field.attrs.iter().fold((0, vec![], vec![]), |(mut sigma_f, mut params_f, mut fields_f), attr| {
                    if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().position(|x| *x == mv.path.segments.first().unwrap().ident.to_string().as_str()) {
                        sigma_f += 1 << attrib;
                        params_f.push(&mv.value);
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
        _ => {
            todo!()
        }
    };

    crate::generator::generate_tokens(sigma, schtruct, &params, &fields)
}

pub struct FilteredField {
    ident: Option<Ident>,
    typ: Type,
    attributes: Vec<Attribute>,
}

impl FilteredField {
    pub fn new(field: Field, attributes: Vec<Attribute>) -> Self {
        Self {
            ident: field.ident,
            typ: field.ty,
            attributes,
        }
    }
}

impl ToTokens for FilteredField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(&self.attributes);
        if let Some(ident) = &self.ident {
            ident.to_tokens(tokens);
            let token = Token!(:)(proc_macro2::Span::call_site());
            token.to_tokens(tokens);
        }
        self.typ.to_tokens(tokens);
    }
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
