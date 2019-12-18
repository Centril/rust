// edition:2015

fn main() {}
fn bar() -> Result<(), ()> {
    panic!()
}

fn foo() -> Result<(), ()> {
   let x = async {
       bar()?;
   };
   Ok(())
}
