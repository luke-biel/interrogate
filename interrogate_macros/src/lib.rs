#![feature(proc_macro_span)]

use convert_case::{Case, Casing};
use itertools::Itertools;
use proc_macro::{SourceFile, TokenStream};
use quote::quote;
use std::time::{SystemTime, UNIX_EPOCH};
use syn::parse::{Parse, ParseStream};
use syn::{
    parse_macro_input, punctuated::Punctuated, Expr, FnArg, Ident, ItemFn, Lit, LitStr, Token,
};

struct TestDefImplArgs {
    arg_generics: Punctuated<Ident, Token![,]>,
}

impl Parse for TestDefImplArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            arg_generics: input.parse_terminated(Ident::parse)?,
        })
    }
}

#[proc_macro]
pub fn test_def_impl(item: TokenStream) -> TokenStream {
    let TestDefImplArgs { arg_generics } = parse_macro_input!(item as TestDefImplArgs);

    let args: Punctuated<_, Token![,]> = arg_generics
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let i = syn::Index::from(i + 2);
            quote! { self.#i.clone() }
        })
        .collect();

    let clones: Punctuated<_, Token![,]> = arg_generics
        .iter()
        .map(|ident| quote! { #ident: Clone })
        .collect();

    let ts = quote! {
        impl<T, #arg_generics> crate::TestDef for (&'static str, T, #arg_generics)
            where
                T: Fn(#arg_generics),
                #clones
        {
            fn run(&self) -> bool {
                self.1.call((#args ,));
                true
            }

            fn name(&self) -> &'static str {
                self.0
            }
        }
    };

    ts.into()
}

struct InterrogateAttr {
    args: Punctuated<Expr, Token![,]>,
    result: Option<InterrogateResult>,
    description: Option<InterrogateDescription>,
}

struct InterrogateResult {
    fat_arrow: Token![=>],
    result: Expr,
}

struct InterrogateDescription {
    semicolon: Token![;],
    text: LitStr,
}

impl Parse for InterrogateAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            args: {
                let mut acc: Punctuated<_, Token![,]> = Punctuated::new();
                let mut last_item_expr = false;
                loop {
                    if input.is_empty() {
                        break;
                    }

                    let lookahead1 = input.lookahead1();

                    if last_item_expr {
                        if lookahead1.peek(Token![,]) {
                            acc.push_punct(input.parse()?);
                            last_item_expr = false;
                        } else {
                            break;
                        }
                    } else {
                        let expr: Expr = match input.parse() {
                            Ok(expr) => expr,
                            Err(_) => break,
                        };
                        acc.push(expr);
                        last_item_expr = true;
                    }
                }
                acc
            },
            result: {
                let lookahead1 = input.lookahead1();
                if lookahead1.peek(Token![=>]) {
                    Some(input.parse()?)
                } else {
                    None
                }
            },
            description: {
                let lookahead1 = input.lookahead1();
                if lookahead1.peek(Token![;]) {
                    Some(input.parse()?)
                } else {
                    None
                }
            },
        })
    }
}

impl Parse for InterrogateResult {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            fat_arrow: input.parse()?,
            result: input.parse()?,
        })
    }
}

impl Parse for InterrogateDescription {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            semicolon: input.parse()?,
            text: input.parse()?,
        })
    }
}

#[proc_macro_attribute]
pub fn fact(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as InterrogateAttr);

    let fun = parse_macro_input!(item as ItemFn);

    let ident = fun.sig.ident.clone();
    let source_file = ident.span().unwrap().source_file();

    let full_name = make_const_name(&ident, &source_file, &attr);
    let test_name = make_test_name(&ident, &source_file, &attr);

    let arg_types: Punctuated<_, Token![,]> = fun
        .sig
        .inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Receiver(_) => panic!("Receiver"),
            FnArg::Typed(typed) => &typed.ty,
        })
        .collect();

    let args = attr.args;

    let ts = quote! {
        #[test_case]
        const #full_name: (&'static str, fn(#arg_types), #arg_types) = (#test_name, #ident, #args);

        #fun
    };

    ts.into()
}

fn make_test_name(ident: &Ident, source_file: &SourceFile, attr: &InterrogateAttr) -> String {
    format!(
        "{}:{} fn {}({})",
        source_file.path().to_string_lossy(),
        ident.span().unwrap().start().line,
        ident,
        attr.args.iter().map(|e| stringify_expr(e)).join(", ")
    )
}

fn make_const_name(ident: &Ident, source_file: &SourceFile, attr: &InterrogateAttr) -> Ident {
    Ident::new(
        &format!(
            "{}_{}_{}_{}",
            source_file
                .path()
                .to_string_lossy()
                .replace(|c| matches!(c, '.' | '/'), "_")
                .to_case(Case::UpperSnake),
            ident.to_string().to_case(Case::UpperSnake),
            attr.args
                .iter()
                .map(|e| stringify_expr(e))
                .join("_")
                .to_case(Case::UpperSnake),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos(),
        ),
        ident.span(),
    )
}

fn stringify_expr(expr: &Expr) -> String {
    match expr {
        Expr::Array(_) => "Array".to_string(),
        Expr::Assign(_) => "Assign".to_string(),
        Expr::AssignOp(_) => "AssignOp".to_string(),
        Expr::Async(_) => "Async".to_string(),
        Expr::Await(_) => "Await".to_string(),
        Expr::Binary(_) => "Binary".to_string(),
        Expr::Block(_) => "Block".to_string(),
        Expr::Box(_) => "Box".to_string(),
        Expr::Break(_) => "Break".to_string(),
        Expr::Call(_) => "Call".to_string(),
        Expr::Cast(_) => "Cast".to_string(),
        Expr::Closure(_) => "Closure".to_string(),
        Expr::Continue(_) => "Continue".to_string(),
        Expr::Field(_) => "Field".to_string(),
        Expr::ForLoop(_) => "ForLoop".to_string(),
        Expr::Group(_) => "Group".to_string(),
        Expr::If(_) => "If".to_string(),
        Expr::Index(_) => "Index".to_string(),
        Expr::Let(_) => "Let".to_string(),
        Expr::Lit(expr_lit) => match &expr_lit.lit {
            Lit::Str(lit_str) => lit_str.value(),
            Lit::ByteStr(lit_bstr) => String::from_utf8_lossy(&lit_bstr.value()).to_string(),
            Lit::Byte(lit_b) => (lit_b.value() as char).to_string(),
            Lit::Char(lit_c) => lit_c.value().to_string(),
            Lit::Int(lit_int) => lit_int.base10_digits().to_string(),
            Lit::Float(lit_float) => lit_float.base10_digits().to_string(),
            Lit::Bool(lit_bool) => lit_bool.value.to_string(),
            Lit::Verbatim(_) => "Literal".to_string(),
        },
        Expr::Loop(_) => "Loop".to_string(),
        Expr::Macro(_) => "Macro".to_string(),
        Expr::Match(_) => "Match".to_string(),
        Expr::MethodCall(_) => "MethodCall".to_string(),
        Expr::Paren(_) => "Paren".to_string(),
        Expr::Path(_) => "Path".to_string(),
        Expr::Range(_) => "Range".to_string(),
        Expr::Reference(_) => "Reference".to_string(),
        Expr::Repeat(_) => "Repeat".to_string(),
        Expr::Return(_) => "Return".to_string(),
        Expr::Struct(_) => "Struct".to_string(),
        Expr::Try(_) => "Try".to_string(),
        Expr::TryBlock(_) => "TryBlock".to_string(),
        Expr::Tuple(_) => "Tuple".to_string(),
        Expr::Type(_) => "Type".to_string(),
        Expr::Unary(_) => "Unary".to_string(),
        Expr::Unsafe(_) => "Unsafe".to_string(),
        Expr::Verbatim(_) => "Verbatim".to_string(),
        Expr::While(_) => "While".to_string(),
        Expr::Yield(_) => "Yield".to_string(),
        Expr::__TestExhaustive(_) => "_".to_string(),
    }
}
