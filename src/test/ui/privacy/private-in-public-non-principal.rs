#![feature(optin_builtin_traits)]

pub trait PubPrincipal {}
auto trait PrivNonPrincipal {}

pub fn leak_dyn_nonprincipal() -> Box<PubPrincipal + PrivNonPrincipal> { loop {} }
//~^ WARN private trait `PrivNonPrincipal` in public interface
//~| WARN this was previously accepted
//~| WARN hard error

#[deny(missing_docs)]
fn container() {
    impl dyn PubPrincipal {
        pub fn check_doc_lint() {} //~ ERROR missing documentation for a method
    }
    impl dyn PubPrincipal + PrivNonPrincipal {
        pub fn check_doc_lint() {} // OK, no missing doc lint
    }
}

fn main() {}
