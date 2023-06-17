use crate::attributes::{AttributeBuilder, TYPE_OPTIONS};
use proc_macro::TokenStream;
use syn::{parse_macro_input, Item, ItemStruct};

mod attributes;
mod generator;

#[proc_macro_attribute]
pub fn assert_proc(tokens: TokenStream, inputs: TokenStream) -> TokenStream {
    let input_tokens = parse_macro_input!(inputs as Item);
    match input_tokens {
        Item::Struct(s) => {
            let validated = validate_fields(&s);
            let validated_tokens = validated.prepare_tokens(s);
            generator::generate_test_functions_for_struct(validated_tokens, tokens)
        }
        _ => todo!(),
    }
}

fn validate_fields(schtruct: &ItemStruct) -> AttributeBuilder {
    let fields: u8 = schtruct
        .attrs
        .iter()
        .filter_map(|attr| {
            let path = attr.path().segments.first().unwrap().ident.to_string();
            TYPE_OPTIONS
                .iter()
                .position(|opt| *opt == path)
                .map(|pos| 1 << pos)
        })
        .sum();

    AttributeBuilder::new(fields)
}

