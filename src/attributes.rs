use proc_macro::TokenStream;
use syn::Meta;
use syn::{Attribute, ItemStruct};

pub const TYPE_OPTIONS: [&str; 1] = ["assert_duplicated"];

pub fn prepare_tokens(schtruct: &ItemStruct) -> TokenStream {
    schtruct.attrs.iter().fold(TokenStream::default(), |mut tokens, attr| {
            if let Meta::NameValue(ref mv) = attr.meta && let Some(attrib) = TYPE_OPTIONS.iter().find(|x| **x == mv.path.segments.first().unwrap().ident.to_string().as_str()) {
            let filtered_tokens = match attrib {
            &"assert_duplicated" => {
                crate::generator::generate_test_stub_for_duplicate(schtruct, &mv.value)
            },
            _ => todo!()
        };

        tokens.extend(filtered_tokens);

        tokens
            } else {
                tokens
            }
})
}

pub fn clean_up(attrs: &mut Vec<Attribute>) {
    // attrs.drain_filter(|attr| attr.path().segments.first().unwrap().ident.to_string().starts_with("assert_"));
    attrs.clear();
}
