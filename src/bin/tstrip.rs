use std::env::args;

use telda2::{self, aalv::obj::{Object, RelocationTable}};

fn main() {
    let arg = args().nth(1).unwrap();

    let mut obj = Object::from_file(&arg).unwrap();
    if obj.entry.is_some() {
        obj.relocation_table = RelocationTable::default();
        obj.symbols.0.clear();
    } else {
        eprintln!("no entry point, so only removing internal symbols");
        obj.symbols.mutate(|name, &mut is_global, _, _| if !is_global { *name = "".into() });
    }
    obj.write_to_file(arg).unwrap();
}
