use proc_macro::TokenStream;
use syn::Meta;
use syn::{Attribute, ItemStruct, Fields};
use core::iter::Extend;

pub const TYPE_OPTIONS: [&str; 2] = [
    "assert_duplicated",
    "assert_field_type",
];

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
        _ => todo!()
    };

    dbg!(&sigma);

    crate::generator::generate_tokens(sigma, schtruct, &params, &fields) 


}

pub fn clean_up(attrs: &mut Vec<Attribute>) {
    // attrs.drain_filter(|attr| attr.path().segments.first().unwrap().ident.to_string().starts_with("assert_"));
    attrs.clear();
}
