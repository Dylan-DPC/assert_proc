#![feature(let_chains)]
#![feature(extract_if)]
#![feature(if_let_guard)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(exact_size_is_empty)]
#![deny(rust_2018_idioms)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::needless_for_each)]
use proc_macro::TokenStream;
use syn::{parse_macro_input, Item};

mod attributes;
mod fragment;
mod generator;

#[proc_macro_attribute]
pub fn assert_proc(tokens: TokenStream, inputs: TokenStream) -> TokenStream {
    let input_tokens = parse_macro_input!(inputs as Item);
    crate::attributes::prepare_tokens(&input_tokens)
}
