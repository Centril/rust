// aux-build:extern-statics.rs

#![allow(unused)]

extern crate extern_statics;
use extern_statics::*;

extern {
    static A: u8;
}

fn main() {
    let a = A; //~ ERROR use of extern static is unsafe
               //~^ WARN this was previously accepted by the compiler
               //~| WARN it will become a hard error
    let ra = &A; //~ ERROR use of extern static is unsafe
                 //~^ WARN this was previously accepted by the compiler
                 //~| WARN it will become a hard error
    let xa = XA; //~ ERROR use of extern static is unsafe
                 //~^ WARN this was previously accepted by the compiler
                 //~| WARN it will become a hard error
    let xra = &XA; //~ ERROR use of extern static is unsafe
                   //~^ WARN this was previously accepted by the compiler
                   //~| WARN it will become a hard error
}
