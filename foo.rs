#![feature(const_generics)]

struct S<const N: usize>([u8; N]);

fn main() {
    let a: S<2> = S([0, 1]);
    let b: S<{_}> = a;

    //let a: [u8; 2] = [0; _];
    //let a: [u8; 1usize + _] = [0, 1];
    //let a: () = _;
    //let a: String = _;
    dbg!(a.0);
}
