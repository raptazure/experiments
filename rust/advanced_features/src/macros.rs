// Declarative Macros with macro_rules! for General Metaprogramming
#[macro_export]
macro_rules! vec {
    ($($x:expr), *) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

// Procedural Macros for Generating Code from Attributes
use proc_macro;

#[some_attribute]
pub fn some_name(input: TokenStream) -> TokenStream {}
// Custom derive Macro -> hello_macro_derive::HelloMacro

// Attribute-like macros
// #[route(GET, "/")]
// fn index() {}
#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream) -> TokenStream {}

// Function-like macros
// let sql = sql!(SELECT * FROM posts WHERE id=1);
#[proc_macro]
pub fn sql(input: TokenStream) -> TokenStream {}
