
use proc_macro::{TokenStream};
use syn::{parse_macro_input, Item, ItemStruct, Ident};

mod generator;


#[proc_macro_attribute]
pub fn assert_proc(tokens: TokenStream, inputs: TokenStream) -> TokenStream {
    let inp = inputs.clone();
    let input_tokens = parse_macro_input!(inputs as Item);
    match input_tokens{ 
        Item::Struct(s) => {
            let validated = validate_fields(s).unwrap_or(inp);
            generator::generate_test_functions_for_struct(validated,tokens)
        },
        _ => todo!()
    }

}

fn validate_fields(shtruct: ItemStruct) -> Option<TokenStream> {
    let mut attributed_fields = shtruct.fields.iter().flat_map(|field| field.attrs.iter().map(|attr| {
        if attr.path().segments.first().unwrap().ident == Ident::new("assert", proc_macro::Span::call_site().into()) { 
            None
        } else {
            todo!()
        }
    }));

    attributed_fields.next().and_then(|fields| fields)
}


