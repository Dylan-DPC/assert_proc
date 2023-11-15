use crate::fragment::Disintegrate;
use crate::proxy::{FilteredField, FilteredMember, MetaParam, ParseableExpr, Structure};
use hasheimer::{oom::OneOrMany, Hasheimer};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Expr, ExprPath, Ident, Item, ItemEnum, ItemStruct};

pub struct TokenGenerator<'a> {
    sigma: u8,
    item: &'a Item,
    exprs: &'a Hasheimer<u8, MetaParam>,
    fields: &'a Hasheimer<u8, FilteredMember>,
    tokens: TokenStream,
}

impl<'a> TokenGenerator<'a> {
    pub fn new(
        sigma: u8,
        item: &'a Item,
        exprs: &'a Hasheimer<u8, MetaParam>,
        fields: &'a Hasheimer<u8, FilteredMember>,
    ) -> Self {
        Self {
            sigma,
            item,
            exprs,
            fields,
            tokens: TokenStream::default(),
        }
    }

    pub fn generate(&self) -> TokenStream {
        match self.sigma {
            1 => self.duplicate(),
            /*
            (2, Item::Struct(schtruct)) => {
                generate_stub_for_type_change(schtruct, fields, exprs.get(&1).unwrap(), 1)
            }
            (2, Item::Enum(enoom)) => {
                todo!()
                // generate_stub_for_enum_data_change(enoom, fields, exprs.get(&1).unwrap(), fields.get(&1).unwrap())
            }

            (5, Item::Struct(schtruct)) => {
                let tokens = generate_stub_for_duplicate_struct(schtruct, fields, exprs.get_if_single(&0u8).unwrap());
                generate_stub_for_value_initialised(schtruct, tokens, fields, exprs)
            }
            */
            _ => todo!("silly"),
        }
    }

    fn duplicate(&self) -> TokenStream {
        let expr = self.exprs.get_if_single(&0).unwrap();
        match self.item {
            Item::Struct(schtruct) => {
                let mod_name = mod_name(&schtruct.ident);
                let Structure {
                    name,
                    attributes,
                    members,
                } = schtruct.get_parts(self.fields);
                let initialiser = match crate::fragment::get_params_for_duplicated(expr) {
                    (name, Some(init)) => quote!(<#name>::#init),
                    (name, _) => quote!(<#name>::default()),
                };

                quote! {
                mod #mod_name {
                    use super::*;

                    #(#attributes)*
                    struct #name{
                        #(#members),*
                    }

                    pub fn foo() {
                        let f = #initialiser;
                    }
                }
                }
            }
            Item::Enum(enoom) => {
                let mod_name = mod_name(&enoom.ident);
                let Structure {
                    name,
                    attributes,
                    members,
                } = enoom.get_parts(self.fields);

                let initialiser = match crate::fragment::get_params_for_duplicated(expr) {
                    (name, Some(init)) => quote!(<#name>::#init),
                    (name, _) => quote!(<#name>::default()),
                };

                quote! {
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
                }
            }

            _ => todo!("unit"),
        }
    }
}

pub fn mod_name(name: &Ident) -> Ident {
    Ident::new(format!("tests_{name}").as_str(), Span::call_site())
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
    quote! {
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
    }
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

pub fn generate_stub_for_value_initialised(
    schtruct: &ItemStruct,
    tokens: TokenStream,
    fields: &Hasheimer<u8, FilteredMember>,
    exprs: &Hasheimer<u8, MetaParam>,
) -> TokenStream {
    todo!()
}

pub fn pgenerate_stub_for_value_initialised(
    schtruct: &ItemStruct,
    tokens: TokenStream,
    fields: &Hasheimer<u8, FilteredMember>,
    exprs: &Hasheimer<u8, MetaParam>,
) -> TokenStream {
    let mod_name = mod_name(&schtruct.ident);
    let Structure {
        name,
        attributes,
        members,
    } = schtruct.get_parts(fields);
    let initialiser =
        match crate::fragment::get_params_for_duplicated(exprs.get_if_single(&0).unwrap()) {
            (name, Some(init)) => quote!(<#name>::#init),
            (name, _) => quote!(<#name>::default()),
        };

    let (fs, es) = match (fields.get(&2), exprs.get(&2)) {
        (
            Some(OneOrMany::Single(FilteredMember::Field(v))),
            Some(OneOrMany::Single(MetaParam::NameValueExpr(e))),
        ) => (vec![v.ident.clone()], vec![ParseableExpr(e.clone())]),
        _ => todo!(),
    };

    quote! {
        // #[cfg(test)]
        mod #mod_name {
            use super::*;

            #(#attributes)*
            struct #name{
                #(#members),*
            }



            // #[test]
            pub fn foo() {
                let ob = #initialiser;
                if (#(ob.#fs),*) != (#(#es),*) {
                   panic!("value in type doesn't match the asserted value")
                }

            }
        }
    }
}
