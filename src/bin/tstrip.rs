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
    input_files: Vec<PathBuf>,

    /// Strip all symbol and relocation tables
    ///
    /// Should only be used on a final binary, since it will no longer be possible to link it.
    /// Without this only internal symbols are wiped
    #[arg(short = 'a', long)]
    all: bool,

    /// Remove all even if no entry point is defined
    #[arg(short, requires = "all")]
    force: bool,
}

fn main() -> ExitCode {
    let Cli { input_files, all, force} = Cli::parse();

    let mut ret = ExitCode::SUCCESS;
    for input_file in input_files {
        match inner_main(all, force, &input_file) {
            Ok(()) => (),
            Err(e) => {
                ret = ExitCode::FAILURE;
                eprintln!("{}: {e}", input_file.display());
            }
        }
    }

    ret
}

fn inner_main(all: bool, force: bool, input_file: &PathBuf) -> Result<(), String> {
    let mut obj = match Object::from_file(input_file) {
        Ok(o) => o,
        Err(e) => {
            return Err(format!("could not read object file: {e}"));
        }
    };

    if all {
        if obj.entry.is_none() && !force {
            return Err(format!("no entry point, will not strip, output object would be neither be linkable nor runnable: use -f to strip anyways"));
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
    obj.write_to_file(input_file)
        .map_err(|e| {
            format!("could write new object: {e}")
        })
}
