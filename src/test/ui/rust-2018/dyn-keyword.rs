// edition:2015
// run-rustfix

#![allow(unused_variables)]
#![deny(keyword_idents)]

fn main() {
    let dyn = (); //~ ERROR dyn
    //~^ WARN this was previously accepted
    //~| WARN hard error in the 2018 edition
}
