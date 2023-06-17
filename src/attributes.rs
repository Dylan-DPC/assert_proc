use proc_macro::TokenStream;
use syn::ItemStruct;

pub const TYPE_OPTIONS: [&str; 1] = ["assert_duplicated"];

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AttributeBuilder {
    options: BitFlags<1>,
}

impl AttributeBuilder {
    pub fn new(n: u8) -> Self {
        AttributeBuilder {
            options: BitFlags::new(n),
        }
    }

    pub fn prepare_tokens(&self, schtruct: ItemStruct) -> TokenStream {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct BitFlags<const N: usize> {
    flags: u8,
}

impl<const N: usize> BitFlags<N> {
    pub fn new(n: u8) -> Self {
        if N > 8 {
            panic!("bitflags are only supported for 8 bits");
        } else {
            BitFlags { flags: 0 }
        }
    }
}
