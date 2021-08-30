#![feature(custom_test_frameworks)]
#![test_runner(interrogate::test_runner)]

use interrogate::fact;

#[fact(0)]
#[fact(120)]
#[fact(32 => 0)]
#[fact(230 ; "this is test")]
fn test_even(value: i32) {
    assert_eq!(value % 2, 0)
}

#[fact(2, 3)]
fn test_two(v1: i32, v2: i32) {

}
