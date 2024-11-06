use proc_macro::TokenStream;
use quote::quote;
use syn::token::Match;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input_orig = input.clone();
    let items = parse_macro_input!(input as syn::Item);
    let args_orig = args.clone();
    let args = proc_macro2::TokenStream::from(args);
    // let args = parse_macro_input!(args as syn::Item);

    match items {
        syn::Item::Enum(_) => {}
        _ => {
            return syn::Error::new_spanned(args, "expected enum or match expression")
                .to_compile_error()
                .into();
        }
    }

    process(items)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn process(input: syn::Item) -> Result<proc_macro2::TokenStream, syn::Error> {
    let enum_items = match input.clone() {
        syn::Item::Enum(e) => e
            .variants
            .iter()
            .map(|v| v.ident.to_string())
            .collect::<Vec<_>>(),
        _ => unreachable!(),
    };

    let mut sorted_enum_items = enum_items.clone();
    sorted_enum_items.sort();

    let mut last_item: Option<&str> = None;
    for orig_item in enum_items.iter() {
        if let Some(last_item) = last_item {
            if orig_item.as_str() < last_item {
                let correct_position = sorted_enum_items
                    .iter()
                    .position(|item| item == orig_item)
                    .unwrap();
                let correct_before = sorted_enum_items.get(correct_position + 1).unwrap();

                let error_item = match input.clone() {
                    syn::Item::Enum(e) => {
                        e.variants
                            .iter()
                            .find(|v| v.ident == orig_item)
                            .unwrap()
                            .clone()
                            .ident
                    }
                    _ => unreachable!(),
                };

                return Err(syn::Error::new_spanned(
                    error_item,
                    format!("{} should sort before {}", orig_item, correct_before),
                ));
            }
        } else {
            last_item = Some(orig_item);
        }
    }

    Ok(proc_macro2::TokenStream::from(quote! {
        #input
    }))
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input_orig = input.clone();
    let items = parse_macro_input!(input as syn::ItemFn);

    // input_orig

    process_check(items)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn process_check(mut input: syn::ItemFn) -> Result<proc_macro2::TokenStream, syn::Error> {
    let mut visitor = MatchVisitor::new();
    visitor.visit_item_fn_mut(&mut input);

    if let Some(err) = visitor.err {
        return Err(err);
    }

    Ok(proc_macro2::TokenStream::from(quote! {
        #input
    }))
}

struct MatchVisitor {
    err: Option<syn::Error>,
}

impl MatchVisitor {
    pub fn new() -> Self {
        Self { err: None }
    }
}

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        // check #[sorted] attribute
        let has_sorted = i.attrs.iter().any(|attr| attr.path().is_ident("sorted"));
        if !has_sorted {
            return;
        }

        // remove #[sorted] attribute
        i.attrs.retain(|attr| !attr.path().is_ident("sorted"));

        let arm_pats = i.arms.iter().map(|a| a.pat.clone()).collect::<Vec<_>>();
        let arm_items = arm_pats
            .iter()
            .filter_map(|p| match p {
                syn::Pat::TupleStruct(t) => Some(t.path.segments.last().unwrap().ident.to_string()),
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut sorted_arm_items = arm_items.clone();
        sorted_arm_items.sort();

        let mut last_item: Option<&str> = None;
        for orig_item in arm_items.iter() {
            if let Some(last_item) = last_item {
                if orig_item.as_str() < last_item {
                    let correct_position = sorted_arm_items
                        .iter()
                        .position(|item| item == orig_item)
                        .unwrap();
                    let correct_before = sorted_arm_items.get(correct_position + 1).unwrap();

                    let error_item = i
                        .arms
                        .iter()
                        .find(|a| {
                            if let syn::Pat::TupleStruct(t) = &a.pat {
                                t.path.segments.last().unwrap().ident.to_string().as_str()
                                    == orig_item
                            } else {
                                false
                            }
                        })
                        .unwrap()
                        .pat
                        .clone();

                    let error_item = match error_item {
                        syn::Pat::TupleStruct(t) => t.path.segments.last().unwrap().ident.clone(),
                        _ => unreachable!(),
                    };

                    self.err = Some(syn::Error::new_spanned(
                        error_item,
                        format!("{} should sort before {}", orig_item, correct_before),
                    ));
                    return;
                }
            } else {
                last_item = Some(orig_item);
            }
        }
    }
}
