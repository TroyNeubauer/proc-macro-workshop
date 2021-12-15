use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_error::{emit_error, proc_macro_error};
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields};

fn is_tt_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 {
            return None;
        }
        let arg = &&p.path.segments[0];
        if arg.ident == "Option" {
            if let syn::PathArguments::AngleBracketed(ref inner_ty) = arg.arguments {
                if inner_ty.args.len() != 1 {
                    return None;
                }
                let inner_ty = inner_ty.args.first().unwrap();
                if let syn::GenericArgument::Type(t) = inner_ty {
                    return Some(t);
                }
            }
        }
    }
    None
}

fn get_vec_ty(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 {
            return None;
        }
        let arg = &&p.path.segments[0];
        if arg.ident == "Vec" {
            if let syn::PathArguments::AngleBracketed(ref inner_ty) = arg.arguments {
                if inner_ty.args.len() != 1 {
                    return None;
                }
                let inner_ty = inner_ty.args.first().unwrap();
                if let syn::GenericArgument::Type(t) = inner_ty {
                    return Some(t);
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
#[proc_macro_error]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let span = input.span();

    let name = input.ident;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let data = match input.data {
        Data::Struct(data) => data,
        _ => {
            emit_error!(span, "Builder derive macros only support structs");
            return proc_macro::TokenStream::new();
        }
    };

    let fields = match data.fields {
        Fields::Named(f) => f,
        _ => {
            emit_error!(
                span,
                "Builder derive macros only support structs with named fields"
            );
            return proc_macro::TokenStream::new();
        }
    };

    let optioned = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if is_tt_option(ty).is_some() || get_vec_ty(ty).is_some() {
            quote! {
                #name: #ty,
            }
        } else {
            quote! {
                #name: std::option::Option<#ty>,
            }
        }
    });

    let none_init = fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: Default::default(),
        }
    });

    let each_methods_raw = fields.named.iter().flat_map(|f| {
        let f_ty = f.ty.clone();
        let f_ident = f.ident.clone();
        f.attrs.iter().filter_map(move |attr| -> Option<(TokenStream, Option<Ident>)> {
            let mut result = None;
            if let Some(inner_vec_ty) = get_vec_ty(&f_ty) {
                if let Ok(syn::Meta::List(meta)) = attr.parse_meta() {
                    if meta.path.segments.len() == 1
                        && meta.path.segments[0].ident == "builder"
                        && meta.nested.len() == 1
                    {
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(value)) =
                            meta.nested.first().unwrap()
                        {
                            if value.path.segments.len() == 1
                                && value.path.segments[0].ident == "each"
                            {
                                match &value.lit {
                                    syn::Lit::Str(lit_str) => {
                                        let method_name = syn::Ident::new(lit_str.value().as_str(), Span::call_site());
                                        let has_same_name = Some(&method_name) == f.ident.as_ref();
                                        result = Some((quote! {
                                            fn #method_name(&mut self, #method_name: #inner_vec_ty) -> &mut Self {
                                                self.#f_ident.push(#method_name);
                                                self
                                            }
                                        }, has_same_name));
                                    }
                                    _ => {
                                        emit_error!(
                                            value.lit.span(),
                                            "using #[builder(each = ...)] requires a string literal"
                                        );
                                    }
                                }
                            } else {
                                emit_error!(
                                    meta.span(),
                                    "expected `builder(each = \"...\")`"
                                )
                            }
                        }
                    }
                }
            }
            result.map(|(tt, skip_big_method)| {
                if skip_big_method {
                    (tt, f.ident.clone())
                } else {
                    (tt, None)
                }
            })
        })
    });

    let mut skip_big_methods = Vec::new();
    let mut each_methods = Vec::new();
    for (tt, ident) in each_methods_raw {
        skip_big_methods.push(ident);
        each_methods.push(tt);
    }

    let setter_methods = fields.named.iter().map(move |f| {
        let name = &f.ident;
        let ty = &f.ty;

        if skip_big_methods.contains(name) {
            return TokenStream::new();
        }

        if get_vec_ty(ty).is_some() {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else if let Some(inner_ty) = is_tt_option(ty) {
            quote! {
                fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
            }
        }
    });

    let built_fields = fields.named.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if is_tt_option(ty).is_some() || get_vec_ty(ty).is_some() {
            quote! {
                #name: self.#name.clone(),
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or_else(|| "Field unset")?,
            }
        }
    });

    let build_methods = quote! {
        pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
            Ok(#name {
                #(#built_fields)*
            })
        }
    };

    let builder_name = Ident::new(format!("{}Builder", name).as_str(), Span::call_site());
    let expanded = quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#none_init)*
                }
            }
        }
        pub struct #builder_name {
            #(#optioned)*
        }

        impl #builder_name {
            #build_methods

            #(#setter_methods)*

            #(#each_methods)*
        }
    };

    proc_macro::TokenStream::from(expanded)
}
