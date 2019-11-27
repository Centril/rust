#![feature(crate_visibility_modifier)]

use super::f; //~ ERROR there are too many initial `super`s

pub(super) struct A; //~ ERROR no parent module for `super` visibility to be relative to
super struct B; //~ ERROR no parent module for `super` visibility to be relative to

fn main() {
}
