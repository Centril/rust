struct S;

impl S {
    static fn f() {}
    //~^ ERROR expected one of `async`, `const`, `crate`
}

fn main() {}
