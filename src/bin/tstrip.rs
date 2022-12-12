use std::env::args;

use telda2::{self, aalv::obj::{Object, ShebangAgnosticObject}};

fn main() {
    let arg = args().nth(1).unwrap();

    let mut obj = ShebangAgnosticObject::from_file(&arg).unwrap();
    strip_obj(&mut obj.obj_mut());
    obj.save(arg).unwrap();
}

fn strip_obj(obj: &mut Object) {
    obj.global_symbols = None;
    obj.internal_symbols = None;
    obj.symbol_reference_table = None;
}
