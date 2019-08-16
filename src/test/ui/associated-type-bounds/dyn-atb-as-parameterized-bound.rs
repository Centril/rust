// Check that a bound of form `Parameterized<dyn ObjWith<Assoc: Copy>>`
// results in `could not find defining uses` for each occurrence.
// Currently, the only situations where this will work is in APIT and RPIT.
//
// FIXME: Feature gate separately since these are probably not the semantics
// we want? Seems like we want input semantics for some of these. // Centril

#![feature(associated_type_bounds)]

// TODO

fn main() {}

trait ObjWith { type Assoc; }

trait Parameterized<P> {}

// Traits:

trait TraitInline { type A: Parameterized<dyn ObjWith<Assoc: Copy>>; }
trait TraitWhere where Self::A: Parameterized<dyn ObjWith<Assoc: Copy>> { type A; }
trait TraitParam<T: Parameterized<dyn ObjWith<Assoc: Copy>>> {}

// Implementations:

trait Foo<T> {}
impl<T: Parameterized<dyn ObjWith<Assoc: Copy>>> Foo<T> for u8 {}
impl<T> Foo<T> for u16 where T: Parameterized<dyn ObjWith<Assoc: Copy>> {}

// ADTs:

struct StructRec<T> where T: Parameterized<dyn ObjWith<Assoc: Copy>> { f: T }
struct StructTup<T>(T) where T: Parameterized<dyn ObjWith<Assoc: Copy>>;
union Union<T: Copy> where T: Parameterized<dyn ObjWith<Assoc: Copy>> { f: T }
enum Enum<T> where T: Parameterized<dyn ObjWith<Assoc: Copy>> { V(T) }

// Functions:

fn foo<T>() where T: Parameterized<dyn ObjWith<Assoc: Copy>> {}




// ^--- Also applies to bounds/constraints inside `fn`, `impl`s headers, ADTs, ...
