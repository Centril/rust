// This test ensures that we do not trample over expression forms in Rust 2015
// which pass and need to continue to pass.

// check-pass
// edition:2015

#![allow(non_camel_case_types)]

fn main() {}

fn call_async_macro() {
    macro_rules! async { () => { "foo" } }
    async!();
    async!().to_string();
}

fn access_async_path() {
    let async = 0;
    drop(async);

    let async = |x| x;
    async(0);
    (async)(0);
}

fn mk_async_literal_no_fields() {
    struct async {}
    impl async {
        fn method(&self) {}
    }
    async {};
    async {}.method();
    let x = async {};
    x.method();
}

fn mk_async_literal_with_field() {
    struct async { field: u8 }
    let field = 42;
    async { field };
    async { field }.field;
    async { field, };
    async { field, }.field;
    async { field: 42 };
    async { field: 42 }.field;
    async { field: 42, };
    async { field: 42, }.field;
}

fn mk_async_literal_with_field_tuple() {
    struct async(u8);
    async { 0: 42 };
    async { 0: 42 }.0;
    async { 0: 42, };
    async { 0: 42, }.0;
}
