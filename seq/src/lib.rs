extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{ExprRange, Ident, Token};

#[allow(dead_code)]
struct Sequence {
    name: Ident,
    range: ExprRange,
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![in]>()?;

        let range: ExprRange = input.parse()?;

        Ok(Sequence { name, range })
    }
}

#[proc_macro]
pub fn seq(_input: TokenStream) -> TokenStream {
    // let Sequence { name, range } = parse_macro_input!(input as Sequence);

    // let stmts: Vec<TokenStream> = Vec::new();

    // let x = range.start.unwrap();

    let expanded = quote! {};

    expanded.into()
}
