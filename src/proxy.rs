use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    punctuated::Punctuated,
    token::{Comma, Eq, Paren},
    Attribute, Expr, ExprCall, ExprPath, Field, Fields, Ident, Meta, Token, Type, Variant,
};

use crate::fragment::MetaExtractedValue;

#[derive(Clone)]
pub struct FilteredField {
    pub ident: Option<Ident>,
    pub typ: Type,
    pub attributes: Vec<Attribute>,
}

impl FilteredField {
    pub fn new(field: Field, attributes: Vec<Attribute>) -> Self {
        Self {
            ident: field.ident,
            typ: field.ty,
            attributes,
        }
    }

    pub fn from_raw(ident: Ident, typ: Type, attributes: Vec<Attribute>) -> Self {
        Self {
            ident: Some(ident),
            typ,
            attributes,
        }
    }

    pub fn with_no_ident(typ: Type, attributes: Vec<Attribute>) -> Self {
        Self {
            ident: None,
            typ,
            attributes,
        }
    }
}

impl From<Field> for FilteredField {
    fn from(field: Field) -> Self {
        Self {
            ident: field.ident,
            typ: field.ty,
            attributes: field.attrs,
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

#[derive(Clone)]
pub struct FilteredVariant {
    pub attributes: Vec<Attribute>,
    pub ident: Ident,
    pub fields: Punctuated<FilteredField, Comma>,
    pub discriminant: Option<(Eq, Expr)>,
}

impl FilteredVariant {
    pub fn new(variant: Variant, attributes: Vec<Attribute>) -> Self {
        let fields = match variant.fields {
            Fields::Named(n) => n
                .named
                .iter()
                .fold(Punctuated::new(), |mut punctuated, field| {
                    let field = field.clone();
                    punctuated.push(FilteredField::with_no_ident(field.ty, field.attrs));
                    punctuated
                }),

            Fields::Unnamed(un) => {
                un.unnamed
                    .iter()
                    .fold(Punctuated::new(), |mut punctuated, field| {
                        let field = field.clone();
                        punctuated.push(FilteredField::with_no_ident(field.ty, field.attrs));
                        punctuated
                    })
            }
            Fields::Unit => Punctuated::new(),
        };

        Self {
            ident: variant.ident,
            fields,
            discriminant: variant.discriminant,
            attributes,
        }
    }

    pub fn from_raw(
        ident: Ident,
        fields: Punctuated<FilteredField, Comma>,
        discriminant: Option<(Eq, Expr)>,
        attributes: Vec<Attribute>,
    ) -> Self {
        Self {
            attributes,
            ident,
            fields,
            discriminant,
        }
    }
}

impl ToTokens for FilteredVariant {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(&self.attributes);
        self.ident.to_tokens(tokens);
        self.fields.iter().for_each(|field| {
            field.to_tokens(tokens);
        });
        if let Some((eq_token, disc)) = &self.discriminant {
            eq_token.to_tokens(tokens);
            disc.to_tokens(tokens);
        }
    }
}

#[derive(Clone)]
pub enum FilteredMember {
    Field(FilteredField),
    Variant(FilteredVariant),
    VariantData(FilteredVariant, Punctuated<FilteredField, Comma>),
}

impl FilteredMember {
    pub fn filter_crate_marker_attributes(self) -> Self {
        match self {
            Self::Field(field) => {
                let attrs = field.attributes.into_iter().filter(|attr| {
                        match &attr.meta {
                        Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => false, 
                            Meta::Path(p) if let Some(p) = p.get_ident() && p.to_string().starts_with("assert_") => false,
                            _ => true
                        }
                    }).collect();

                FilteredMember::Field(FilteredField::from_raw(
                    field.ident.unwrap(),
                    field.typ,
                    attrs,
                ))
            }

            Self::Variant(variant) => {
                let attrs = variant.attributes.into_iter().filter(|attr| {
            match &attr.meta {
                        Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => false, 
                            Meta::Path(p) if let Some(p) = p.get_ident() && p.to_string().starts_with("assert_") => false,
                            _ => true
            }
                    }).collect();

                FilteredMember::Variant(FilteredVariant::from_raw(
                    variant.ident,
                    variant.fields,
                    variant.discriminant,
                    attrs,
                ))
            }

            Self::VariantData(variant, fields) => {
                let attrs = variant.attributes.into_iter().filter(|attr| {
                match &attr.meta {
                    Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => false,
                    Meta::Path(p) if let Some(p) = p.get_ident() && p.to_string().starts_with("assert_") => false,
                    _ => true

                }
            }).collect();

                FilteredMember::VariantData(
                    FilteredVariant::from_raw(
                        variant.ident,
                        variant.fields,
                        variant.discriminant,
                        attrs,
                    ),
                    fields,
                )
            }
        }
    }
}

impl ToTokens for FilteredMember {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Field(ff) => {
                ff.to_tokens(tokens);
            }
            Self::Variant(fv) => {
                fv.to_tokens(tokens);
            }

            Self::VariantData(variant, vd) => {
                tokens.append_all(&variant.attributes);
                variant.ident.to_tokens(tokens);
                let token = syn::token::Paren(proc_macro2::Span::call_site());
                token.surround(tokens, |tok| vd.to_tokens(tok));
            }
        }
    }
}

pub struct Structure<'a, I>
where
    I: Iterator<Item = Attribute> + 'a,
{
    pub name: &'a Ident,
    pub attributes: I,
    pub members: Vec<FilteredMember>,
}

impl<'a, I> Structure<'a, I>
where
    I: Iterator<Item = Attribute> + 'a,
{
    pub fn new(name: &'a Ident, attributes: I, members: Vec<FilteredMember>) -> Self {
        Self {
            name,
            attributes,
            members,
        }
    }
}

#[derive(Clone)]
pub struct ParseableExpr(pub Expr);

impl ToTokens for ParseableExpr {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match &self.0 {
            Expr::Call(ExprCall { func, args, .. }) => {
                func.to_tokens(tokens);
                let token = Paren(Span::call_site());
                token.surround(tokens, |tok| args.to_tokens(tok));
            }
            _ => todo!(),
        }
    }
}

#[derive(Clone)]
pub enum MetaParam {
    ListTokens(TokenStream),
    NameValueExpr(Expr),
    Path,
}

impl MetaParam {
    pub fn ident(&self) -> Option<MetaExtractedValue<'_>> {
        match self {
            Self::ListTokens(lt) => crate::fragment::find_value_in_token(lt.clone(), "name"),
            Self::NameValueExpr(Expr::Path(ExprPath { path: p, .. })) => p.get_ident().map(MetaExtractedValue::ExprIdent),
            Self::NameValueExpr(Expr::Call(call)) if let Expr::Path(ExprPath { path, .. }) = call.func.as_ref() => path.get_ident().map(MetaExtractedValue::ExprIdent),
            _ => todo!(),
        }
    }
}
