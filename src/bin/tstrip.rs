use std::{path::PathBuf, process::ExitCode};

use telda2::{
    self,
    aalv::obj::{Object, RelocationTable},
};

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
/// By default, only debug symbols will be stripped
struct Cli {
    /// Object file to strip
    #[arg(required = true)]
    input_file: PathBuf,

    /// Strip everything
    ///
    /// Should only be used on a final binary, since it will no longer be possible to link it
    #[arg(short = 'a', long)]
    all: bool,
}

fn main() -> ExitCode {
    let Cli { input_file, all } = Cli::parse();

    let mut obj = match Object::from_file(&input_file) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("could not read object file: {e}");

            return ExitCode::FAILURE;
        }
    };

    if all {
        if obj.entry.is_none() {
            eprintln!("warning: no entry point, this object will neither be linkable nor runnable");
        }
        obj.relocation_table = RelocationTable::default();
        obj.symbols.0.clear();
    } else {
        obj.symbols.mutate(|name, &mut is_global, _, _| {
            if !is_global {
                *name = "".into()
            }
        });
    }
    match obj.write_to_file(&input_file) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("could write new object: {e}");

            ExitCode::FAILURE
        }
    }
}
