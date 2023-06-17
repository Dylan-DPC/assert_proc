use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemStruct};

pub fn generate_test_functions_for_struct(
    validated: TokenStream,
    tokens: TokenStream,
) -> TokenStream {
    let item = parse_macro_input!(validated as ItemStruct);
    let struct_item = &item.ident;
    let value = if tokens.is_empty() {
        quote!(<#struct_item>::default())
    } else {
        tokens.into()
    };
    TokenStream::from(quote! {
        #item
        mod tests {

            use super::*;

            // #[test]
            pub fn foo() {
            let value = #value;
            }
        }

    })
}
