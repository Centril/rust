// This test documents that `type Out = Box<dyn Bar<Assoc: Bound>>;`
// is allowed and will permit the opaque `type Out` to unify with
// a type where `<TheType as Bar>::Assoc: Bound`.
//
// FIXME(rust-lang/lang): I think this behavior is logical if we want to allow
// `dyn Trait<Assoc: Bound>` but we should decide if we want that. // Centril

// check-pass

#![feature(associated_type_bounds)]

trait Bar { type Assoc; }

trait Thing {
    type Out;
    fn func() -> Self::Out;
}

fn accept_box_dyn_assoc_is_copy<T: Copy>(_: Box<dyn Bar<Assoc = T>>) {}
struct AssocIsCopy;
impl Bar for AssocIsCopy { type Assoc = u8; }
impl Thing for AssocIsCopy {
    type Out = Box<dyn Bar<Assoc: Copy>>;

    fn func() -> Self::Out {
        Box::new(AssocIsCopy)
    }
}

fn accept_box_dyn_assoc_is_static<T: 'static>(_: Box<dyn Bar<Assoc = T>>) {}
struct AssocIsStatic;
impl Bar for AssocIsStatic { type Assoc = u8; }
impl Thing for AssocIsStatic {
    type Out = Box<dyn Bar<Assoc: 'static>>;

    fn func() -> Self::Out {
        Box::new(AssocIsStatic)
    }
}

trait TraitWithLifetime<'a> {}
impl TraitWithLifetime<'_> for u8 {}
fn accept_box_dyn_assoc_is_forall<T: for<'a> TraitWithLifetime<'a>>(_: Box<dyn Bar<Assoc = T>>) {}
struct AssocIsForall;
impl Bar for AssocIsForall { type Assoc = u8; }
impl Thing for AssocIsForall {
    type Out = Box<dyn Bar<Assoc: for<'a> TraitWithLifetime<'a>>>;

    fn func() -> Self::Out {
        Box::new(AssocIsForall)
    }
}

use std::ops::Add;
fn accept_box_dyn_assoc_is_nested<T>(_: Box<dyn Bar<Assoc = T>>)
where
    T: Clone + Iterator,
    T::Item: Add<u8>,
    <T::Item as Add<u8>>::Output: Into<u8>,
{}
#[derive(Clone)]
struct SatisfiedNestedBounds;
impl Iterator for SatisfiedNestedBounds {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> { None }
}
struct AssocIsNested;
impl Bar for AssocIsNested { type Assoc = SatisfiedNestedBounds; }
impl Thing for AssocIsNested {
    type Out = Box<dyn Bar<Assoc: Clone + Iterator<Item: Add<u8, Output: Into<u8>>>>>;

    fn func() -> Self::Out {
        Box::new(AssocIsNested)
    }
}

fn main() {
    accept_box_dyn_assoc_is_copy(<AssocIsCopy as Thing>::func());
    accept_box_dyn_assoc_is_static(<AssocIsStatic as Thing>::func());
    accept_box_dyn_assoc_is_forall(<AssocIsForall as Thing>::func());
    accept_box_dyn_assoc_is_nested(<AssocIsNested as Thing>::func());
}
