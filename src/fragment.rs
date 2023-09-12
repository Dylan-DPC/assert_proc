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
}

#[derive(Clone)]
pub struct FilteredVariant {
    pub attributes: Vec<Attribute>,
    ident: Ident,
    fields: Fields,
    discriminant: Option<(Eq, Expr)>,
}

impl FilteredVariant {
    pub fn new(variant: Variant, attributes: Vec<Attribute>) -> Self {
        Self {
            ident: variant.ident,
            fields: variant.fields,
            discriminant: variant.discriminant,
            attributes,
        }
    }
}

#[derive(Clone)]
pub enum FilteredMember {
    Field(FilteredField),
    Variant(FilteredVariant),
}

impl ToTokens for FilteredMember {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Field(ff) => {
                tokens.append_all(&ff.attributes);
                if let Some(ident) = &ff.ident {
                    ident.to_tokens(tokens);
                    let token = Token!(:)(proc_macro2::Span::call_site());
                    token.to_tokens(tokens);
                }
                ff.typ.to_tokens(tokens);
            }
            Self::Variant(fv) => {
                tokens.append_all(&fv.attributes);
                fv.ident.to_tokens(tokens);
                fv.fields.to_tokens(tokens);
                if let Some((eq_token, disc)) = &fv.discriminant {
                    eq_token.to_tokens(tokens);
                    disc.to_tokens(tokens);
                }
            }
        }
    }
}

pub trait Disintegrate {
    fn get_parts(
        &self,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        impl Iterator<Item = FilteredMember> + '_,
    );
}

impl Disintegrate for ItemStruct {
    fn get_parts(
        &self,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        impl Iterator<Item = FilteredMember> + '_,
    ) {
        let struct_name = &self.ident;

        let attributes = crate::attributes::filter_attributes(&self.attrs);

        let fields = match &self.fields {
            Fields::Named(f) => &f.named,
            Fields::Unnamed(f) => &f.unnamed,
            Fields::Unit => todo!(),
        };

        let fields = fields.iter().map(|field| {
        let attr = field.attrs.iter().filter_map(|attr| {
        match &attr.meta {
            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::Path(p) if let Some(path) = p.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::Path(p) if let Some(p) = p.get_ident() => {
                Some(attr.clone())
            }

            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() => {
                Some(attr.clone())
            },


        _ => {
            todo!()
        }
        }
        });

        FilteredMember::Field(FilteredField::new(field.clone(), attr.collect()))
    });

        (struct_name, attributes, fields)
    }
}

impl Disintegrate for ItemEnum {
    fn get_parts(
        &self,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        impl Iterator<Item = FilteredMember> + '_,
    ) {
        let enum_name = &self.ident;
        let attributes = crate::attributes::filter_attributes(&self.attrs);

        let variants = self.variants.iter().map(|variant| {
            let attr = variant.attrs.iter().filter_map(|attr| {
                match &attr.meta {
                    Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() && path.to_string().starts_with("assert_")=> {
                        None
                    }

            Meta::Path(p) if let Some(path) = p.get_ident() && path.to_string().starts_with("assert_") => {
                None
            }

            Meta::Path(p) if let Some(p) = p.get_ident() => {
                Some(attr.clone())
            }

            Meta::NameValue(nv) if let Some(path) = nv.path.get_ident() => {
                Some(attr.clone())
            },


        _ => {
            todo!()
        }
        }
            });

        FilteredMember::Variant(FilteredVariant::new(variant.clone(), attr.collect()))
        });
        (enum_name, attributes, variants)
    }
}

pub fn param_from_expr(expr: &Expr) -> &Ident {
    match expr {
        Expr::Path(ExprPath { path: p, .. }) => p.get_ident().unwrap(),

        _ => {
            todo!()
        }
    }
}
