crate struct Bender { //~ ERROR short-hand visibility modifier is experimental
    earth: bool,
    fire: bool,
    air: bool,
    water: bool,
}

mod container {
    super struct Foo; //~ ERROR short-hand visibility modifier is experimental
}

fn main() {}
