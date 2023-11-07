use crate::proxy::{FilteredField, FilteredMember, FilteredVariant, MetaParam, Structure};
use hasheimer::{oom::OneOrMany, Hasheimer};
use proc_macro2::{TokenStream as TokenStream2, TokenTree};
use quote::ToTokens;
use syn::{Attribute, Ident, ItemEnum, ItemStruct, Meta};

pub trait Disintegrate {
    fn get_parts<'a>(
        &'a self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> Structure<'a, impl Iterator<Item = Attribute> + '_>;
}

impl Disintegrate for ItemStruct {
    #[allow(clippy::needless_for_each)]
    fn get_parts<'a>(
        &'a self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> Structure<'a, impl Iterator<Item = Attribute> + '_> {
        let struct_name = &self.ident;

        let attributes = crate::attributes::filter_attributes(&self.attrs);
        let fields = fields.iter().fold(Vec::new(), |mut fields, (_, field)| {
            match field {
                OneOrMany::Single(f @ FilteredMember::Field(_)) => {
                    fields.push(f.clone().filter_crate_marker_attributes());
                }

                OneOrMany::Many(v) => {
                    v.iter().for_each(|field| {
                        fields.push(field.clone().filter_crate_marker_attributes());
                    });
                }

                OneOrMany::Single(_) => todo!("one or many is the other single"),
            }
            fields
        });

        Structure::new(struct_name, attributes, fields)
    }
}

impl Disintegrate for ItemEnum {
    #[allow(clippy::needless_for_each)]
    fn get_parts<'a>(
        &'a self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> Structure<'a, impl Iterator<Item = Attribute> + '_> {
        let enum_name = &self.ident;

        let attributes = crate::attributes::filter_attributes(&self.attrs);
        let fields: Vec<FilteredMember> =
            fields.iter().fold(Vec::new(), |mut fields, (_, field)| {
                match field {
                    OneOrMany::Single(f @ FilteredMember::Field(_)) => {
                        fields.push(f.clone().filter_crate_marker_attributes());
                    }

                    OneOrMany::Many(v) => {
                        v.iter().for_each(|field| {
                            fields.push(field.clone().filter_crate_marker_attributes());
                        });
                    }

                    OneOrMany::Single(_) => todo!("you as well"),
                }
                fields
            });

        Structure::new(enum_name, attributes, fields)
    }
}

pub trait FilterAttributes {
    fn filter_crate_marker_attributes(self) -> FilteredMember;
}

impl FilterAttributes for FilteredField {
    fn filter_crate_marker_attributes(self) -> FilteredMember {
        let attrs = self.attributes.into_iter().filter(|attr| {
                        match &attr.meta {
                        Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => false, 
                            Meta::Path(p) if let Some(p) = p.get_ident() && p.to_string().starts_with("assert_") => false,
                            _ => true
                        }
                    }).collect();

        FilteredMember::Field(FilteredField::from_raw(
            self.ident.unwrap(),
            self.typ,
            attrs,
        ))
    }
}

impl FilterAttributes for FilteredVariant {
    fn filter_crate_marker_attributes(self) -> FilteredMember {
        let attrs = self.attributes.into_iter().filter(|attr| {
            match &attr.meta {
                        Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => false, 
                            Meta::Path(p) if let Some(p) = p.get_ident() && p.to_string().starts_with("assert_") => false,
                            _ => true
            }
                    }).collect();

        FilteredMember::Variant(FilteredVariant::from_raw(
            self.ident,
            self.fields,
            self.discriminant,
            attrs,
        ))
    }
}
#[derive(Clone, Debug)]
pub enum MetaExtractedValue<'a> {
    ExprIdent(&'a Ident),
    ListToken(Vec<TokenTree>),
}

impl ToTokens for MetaExtractedValue<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::ExprIdent(id) => id.to_tokens(tokens),
            Self::ListToken(lt) => lt.iter().for_each(|t| t.to_tokens(tokens)),
        }
    }
}
#[derive(Default, Clone, Copy, Debug)]
enum ParseState {
    #[default]
    Empty,
    FoundKey,
    ValueStrip,
    Seek,
    Wind,
}

pub fn find_value_in_token<'a, S>(tokens: TokenStream2, key: S) -> Option<MetaExtractedValue<'a>>
where
    S: AsRef<str> + std::fmt::Debug + 'a,
{
    let tok_iter = tokens.into_iter();

    let (_, buffer, found) = tok_iter.fold((ParseState::default(), vec![], false), |(state, mut buffer, found, ), tok| {

        match (state, tok) {
            (ParseState::Wind, _) => (state, buffer, found),
            (ParseState::Empty, TokenTree::Ident(id)) if id == key => (ParseState::FoundKey, vec![], true),
            (ParseState::Empty, TokenTree::Ident(_)) => (ParseState::Seek, vec![], found),
            (ParseState::Empty, TokenTree::Punct(_)) => (ParseState::Seek, buffer, found),
            (ParseState::FoundKey, TokenTree::Punct(pun)) if pun.as_char() == '=' => (ParseState::ValueStrip, buffer, found),
            (ParseState::ValueStrip, TokenTree::Punct(pun)) if pun.as_char() == '=' => {
                let _ = buffer.pop();
                (ParseState::Wind, buffer, found)
            },
            (ParseState::ValueStrip, tauk)  => {
                buffer.push(tauk);
                (ParseState::ValueStrip, buffer, found)
            },
            (ParseState::Seek, TokenTree::Ident(ref id)) if buffer.is_empty() => {
                buffer.push(TokenTree::Ident(id.clone()));
                (ParseState::Seek, buffer, found)
            },

            (ParseState::Seek, TokenTree::Ident(ref id)) if buffer.len() == 1 => {
                buffer[0] = TokenTree::Ident(id.clone());
                (ParseState::Seek, buffer, found)
            },
            (ParseState::Seek, TokenTree::Punct(pun)) if pun.as_char() == '=' && buffer.len() <= 1 && let Some(TokenTree::Ident(id)) = buffer.get(0) && *id == key => {
                buffer.clear();
                (ParseState::ValueStrip, buffer, true)
            },
            (ParseState::Seek, TokenTree::Punct(pun)) if pun.as_char() == '=' && buffer.len() <= 1 => {
                (ParseState::Seek, buffer, found)
            },
            _ => panic!("malformed attribute"),
        }
    });

    found.then_some(MetaExtractedValue::ListToken(buffer))
}

pub fn get_params_for_duplicated(
    expr: &MetaParam,
) -> (MetaExtractedValue<'_>, Option<MetaExtractedValue<'_>>) {
    let duplicated_name = expr.ident();
    let initialiser = if let MetaParam::ListTokens(lt) = expr {
        find_value_in_token(lt.clone(), "initializer")
    } else {
        None
    };

    (duplicated_name.unwrap(), initialiser)
}
