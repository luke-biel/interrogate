#![feature(fn_traits)]

use interrogate_macros::test_def_impl;

pub use interrogate_macros::fact;

pub trait TestDef {
    fn run(&self) -> bool;
    fn name(&self) -> &'static str;
}

test_def_impl!(T1);
test_def_impl!(T1, T2);
test_def_impl!(T1, T2, T3);
test_def_impl!(T1, T2, T3, T4);
test_def_impl!(T1, T2, T3, T4, T5);
test_def_impl!(T1, T2, T3, T4, T5, T6);
test_def_impl!(T1, T2, T3, T4, T5, T6, T7);
test_def_impl!(T1, T2, T3, T4, T5, T6, T7, T8);
test_def_impl!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
test_def_impl!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);

pub fn test_runner(items: &[&dyn TestDef]) {
    for item in items {
        print!("{}: ", item.name());
        if item.run() {
            println!("Success");
        } else {
            println!("Failure");
        }
    }
}
