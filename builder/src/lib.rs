use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens as _};
use syn::{
    parse2, parse_macro_input, spanned::Spanned as _, AngleBracketedGenericArguments, Attribute,
    Data, DeriveInput, Expr, GenericArgument, Ident, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let builder_ident = format_ident!("{}Builder", input.ident);

    let builder_data = optional_fields(&input.data);

    let name = input.ident.clone();

    // dbg!(&input);

    let args = builder_args(&builder_data);

    // eprintln!("TOKENS: {}", args);

    let builder_struct = DeriveInput {
        ident: builder_ident.clone(),
        vis: input.vis,
        generics: input.generics,
        data: builder_data,
        attrs: input.attrs,
    };

    let field_funcs = match builder_field_funcs(&input.data) {
        Ok(t) => t,
        Err(e) => e.to_compile_error(),
    };

    let build_func = builder_func(&name, &builder_struct.data);

    let derived_impl = builder_impl(&builder_ident, field_funcs, build_func);

    let expanded = quote! {
        #builder_struct

        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #args
                }
            }
        }

        #derived_impl
    };

    // eprintln!("TOKENS: {}", expanded);

    proc_macro::TokenStream::from(expanded)
}

fn optional_fields(data: &Data) -> Data {
    match *data {
        Data::Struct(ref data) => {
            let mut new_data = data.clone();
            let _: Vec<()> = new_data
                .fields
                .iter_mut()
                .map(|f| {
                    let t = &f.ty;
                    let tokens = quote_spanned! {f.span() =>
                        core::option::Option<#t>
                    };

                    f.ty = parse2(tokens).unwrap();
                })
                .collect();
            Data::Struct(new_data)
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn builder_args(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                // As we enclose everything in an option type we need to get the inner type here
                let inner = get_inner_type(ty).unwrap();
                let mut initial_val = quote! {None};
                if is_option_type(inner) {
                    initial_val = quote! {Some(None)};
                }
                quote_spanned! {f.span() =>
                    #name: #initial_val,
                }
            })
            .collect(),
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn builder_impl(name: &Ident, funcs: TokenStream, build_func: TokenStream) -> TokenStream {
    quote! {
        impl #name {

            #funcs

            #build_func
        }
    }
    .into()
}

fn builder_field_funcs(data: &Data) -> syn::Result<TokenStream> {
    let funcs = match data {
        Data::Struct(ref data) => data
            .fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                if is_option_type(ty) {
                    let inner = {
                        let ty = get_inner_type(ty);
                        match ty {
                            Ok(ty) => ty.into_token_stream(),
                            Err(e) => e.to_compile_error(),
                        }
                    };
                    quote_spanned! {f.span() =>
                        fn #name(&mut self, #name: #inner) -> &mut Self {
                            self.#name = Some(Some(#name));
                            self
                        }
                    }
                } else {
                    quote_spanned! {f.span() =>
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                }
            })
            .collect::<TokenStream>(),
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };
    Ok(funcs)
}

fn builder_func(derive_ident: &Ident, data: &Data) -> TokenStream {
    let field_args = match data {
        Data::Struct(ref data) => data
            .fields.iter().map(|f| {
                let name = &f.ident;
                let error_name = stringify!(name);
                quote_spanned! {f.span() =>
                    #name: self.#name.clone().ok_or_else(|| {
                            let tmp: Box<dyn std::error::Error> = Box::from(format!("{} was not set", #error_name));
                            tmp
                        }
                    )?,
                }
            }).collect::<TokenStream>(),
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };
    quote! {
        pub fn build(&mut self) -> Result<#derive_ident, Box<dyn std::error::Error>> {
            use core::result::Result::Ok;
            Ok(
                #derive_ident {
                    #field_args
                }
            )
        }
    }
    .into()
}

fn is_option_type(ty: &Type) -> bool {
    match ty {
        Type::Path(TypePath { path, qself: _ }) => {
            for val in path.segments.iter().map(|seg| {
                if seg.ident.to_string().ends_with("Option") {
                    return true;
                }
                false
            }) {
                if val {
                    return true;
                }
            }
            false
        }
        _ => unimplemented!(),
    }
}

fn get_inner_type(ty: &Type) -> syn::Result<&Type> {
    match ty {
        Type::Path(TypePath { path, qself: _ }) => match path.segments.last().unwrap().arguments {
            syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref args,
                colon2_token: _,
                gt_token: _,
                lt_token: _,
            }) => {
                for arg in args {
                    match arg {
                        GenericArgument::Type(t) => return Ok(t),
                        _ => unimplemented!(),
                    }
                }
                Err(syn::Error::new(
                    Span::call_site(),
                    "expected a type declaration",
                ))
            }
            _ => Err(syn::Error::new(
                Span::call_site(),
                "expected a type declaration",
            )),
        },
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected a type declaration",
        )),
    }
}

fn parse_attrs(attrs: &Vec<Attribute>) -> syn::Result<()> {
    for attr in attrs {
        if attr.path().is_ident("builder") {
            match attr.parse_args::<Expr>()? {
                Expr::Assign(_) => {},
                _ => 
            }
        }
    }
    Ok(())
}
