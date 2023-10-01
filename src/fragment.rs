use hasheimer::{oom::OneOrMany, Hasheimer};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    token::Eq, Attribute, Expr, ExprPath, Field, Fields, Ident, ItemEnum, ItemStruct, Meta, Token,
    Type, Variant,
};

#[derive(Clone)]
pub struct FilteredField {
    pub ident: Option<Ident>,
    typ: Type,
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
    pub fields: Vec<FilteredField>,
    discriminant: Option<(Eq, Expr)>,
}

impl FilteredVariant {
    pub fn new(variant: Variant, attributes: Vec<Attribute>) -> Self {
        let fields = match variant.fields {
            Fields::Named(n) => n
                .named
                .iter()
                .map(|field| {
                    let field = field.clone();
                    FilteredField::with_no_ident(field.ty, field.attrs)
                })
                .collect(),
            Fields::Unnamed(un) => un
                .unnamed
                .iter()
                .map(|field| field.clone().into())
                .collect(),
            Fields::Unit => vec![],
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
        fields: Vec<FilteredField>,
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
    VariantData(FilteredVariant, Vec<FilteredField>),
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
                dbg!(&variant.ident);
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
                vd.iter().for_each(|data| {
                    tokens.append_all(&data.attributes);
                    data.ident.to_tokens(tokens);
                    if let Some(ident) = &data.ident {
                        ident.to_tokens(tokens);
                        let token = Token!(:)(proc_macro2::Span::call_site());
                        token.to_tokens(tokens);
                    }
                });
            }
        }
    }
}

pub trait Disintegrate {
    fn get_parts<'a>(
        &'a self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> (
        &'a syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        Vec<FilteredMember>,
    );
}

impl Disintegrate for ItemStruct {
    #[allow(clippy::needless_for_each)]
    fn get_parts<'a>(
        &'a self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        Vec<FilteredMember>,
    ) {
        let struct_name = &self.ident;

        let attributes = crate::attributes::filter_attributes(&self.attrs);
        let fields = fields.iter().fold(Vec::new(), |mut fields, (k, field)| {
            match field {
                OneOrMany::Single(f @ FilteredMember::Field(_)) => {
                    fields.push(f.clone().filter_crate_marker_attributes());
                }

                OneOrMany::Many(v) => {
                    v.iter().for_each(|field| {
                        fields.push(field.clone().filter_crate_marker_attributes());
                    });
                }

                OneOrMany::Single(_) => todo!(),
            }
            fields
        });

        (struct_name, attributes, fields)
    }
}

impl Disintegrate for ItemEnum {
    #[allow(clippy::needless_for_each)]
    fn get_parts(
        &self,
        fields: &Hasheimer<u8, FilteredMember>,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        Vec<FilteredMember>,
    ) {
        let enum_name = &self.ident;

        let attributes = crate::attributes::filter_attributes(&self.attrs);
        let fields: Vec<FilteredMember> =
            fields.iter().fold(Vec::new(), |mut fields, (k, field)| {
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

        (enum_name, attributes, fields)
    }
}

pub fn param_from_expr(expr: &Expr) -> &Ident {
    match expr {
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident().unwrap(),

        _ => {
            todo!("if you reached here turn back")
        }
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
