#![feature(custom_test_frameworks)]
#![test_runner(interrogate::test_runner)]

use interrogate::interrogate;

#[interrogate(0)]
#[interrogate(120)]
fn test_even(value: i32) {
    assert_eq!(value % 2, 0)
}
