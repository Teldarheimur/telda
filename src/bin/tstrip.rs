use std::env::args;

use telda2::{self, aalv::obj::Object};

fn main() {
    let arg = args().nth(1).unwrap();

    let mut obj = Object::from_file(&arg).unwrap();
    obj.global_symbols = None;
    obj.internal_symbols = None;
    obj.symbol_reference_table = None;
    obj.write_to_file(arg).unwrap();
}
