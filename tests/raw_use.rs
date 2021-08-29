#![feature(custom_test_frameworks)]
#![test_runner(interrogate::test_runner)]

use interrogate::fact;

#[fact(0)]
#[fact(120)]
fn test_even(value: i32) {
    assert_eq!(value % 2, 0)
}
