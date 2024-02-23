use std::{
    fs::File, io::{self, Write}, path::PathBuf, process::ExitCode
};

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
/// Creates an archive of object files that can be provided to a linker
/// that will skip any objects in the archive for which no reference exists
struct Cli {
    /// Input object files to collect into anarchive
    #[arg(required = true)]
    input_files: Vec<PathBuf>,
    #[arg(short, required = true)]
    /// Output archive, conventially with the extension .savn
    output_file: PathBuf,
}

fn main() -> ExitCode {
    match tar_main() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("could not make archive: {e}");

            ExitCode::FAILURE
        }
    }
}

fn tar_main() -> Result<(), io::Error> {
    let Cli {
        input_files,
        output_file,
    } = Cli::parse();

    let mut out = File::create(output_file)?;
    out.write_all("álvasavn\n".as_bytes())?;

    for input_file in input_files {
        let mut input = File::open(input_file)?;
        io::copy(&mut input, &mut out)?;
    }
    out.sync_all()?;

    Ok(())
}
