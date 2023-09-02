use quote::{ToTokens, TokenStreamExt};
use syn::{Attribute, Expr, ExprPath, Field, Fields, Ident, ItemStruct, Meta, Token, Type};

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

pub trait DisIntegrate {
    fn get_parts(
        &self,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        impl Iterator<Item = FilteredField> + '_,
    );
}

impl DisIntegrate for ItemStruct {
    fn get_parts(
        &self,
    ) -> (
        &syn::Ident,
        impl Iterator<Item = Attribute> + '_,
        impl Iterator<Item = FilteredField> + '_,
    ) {
        let struct_name = &self.ident;

        let attributes = self.attrs.iter().filter_map(|attr| {
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
    });

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

        FilteredField::new(field.clone(), attr.collect())
    });

        (struct_name, attributes, fields)
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
