struct Foo;

impl Foo {
    #[stable(feature = "rust1", since = "1.0.0")]
} //~ ERROR expected one of `async`, `const`, `crate`

fn main() {}
