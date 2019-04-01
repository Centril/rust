// run-rustfix

#![feature(rust_2018_preview, crate_visibility_modifier)]
#![deny(absolute_paths_not_starting_with_crate)]

mod foo {
    crate trait Foo {
        type Bar;
    }

    crate struct Baz { }

    impl Foo for Baz {
        type Bar = ();
    }
}


fn main() {
    let _: <foo::Baz as ::foo::Foo>::Bar = ();
    //~^ ERROR absolute paths must start with
    //~| WARN this was previously accepted
    //~| WARN it will become a hard error

    let _: <::foo::Baz as foo::Foo>::Bar = ();
    //~^ ERROR absolute paths must start with
    //~| WARN this was previously accepted
    //~| WARN it will become a hard error
}
