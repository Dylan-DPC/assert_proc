#![feature(let_chains)]
#![feature(extract_if)]
#![feature(if_let_guard)]
#![deny(rust_2018_idioms)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, Item};

mod attributes;
mod generator;

#[proc_macro_attribute]
pub fn assert_proc(tokens: TokenStream, inputs: TokenStream) -> TokenStream {
    let mut input_tokens = parse_macro_input!(inputs as Item);
    match input_tokens {
        Item::Struct(ref mut s) => crate::attributes::prepare_tokens(s),
        _ => todo!(),
    }
}
