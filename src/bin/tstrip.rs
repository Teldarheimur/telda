use std::env::args;

use telda2::{self, aalv::obj::Object};

fn main() {
    let arg = args().nth(1).unwrap();

    let mut obj = Object::from_file(&arg).unwrap();
    obj.symbols.mutate(|name, &mut is_global, _, _| if !is_global { *name = "".into() });
    obj.write_to_file(arg).unwrap();
}
