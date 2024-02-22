use std::{
    collections::{HashMap, VecDeque},
    io::{stdin, stdout, Write},
    path::PathBuf,
    process::ExitCode,
};

use telda2::{
    aalv::obj::{Object, SymbolDefinition}, blf4::*, disassemble::disassemble_instruction, machine::Machine, mem::{Io, LazyMain}
};

struct DbgIo {
    in_buf: VecDeque<u8>,
    out_buf: Vec<u8>,
}

impl Io for DbgIo {
    fn write(&mut self, _addr: u8, val: u8) {
        if val == b'\n' {
            print!("STDOUT line: ");
            std::io::stdout()
                .write_all(&self.out_buf)
                .expect("stdout failed");
            println!();
            self.out_buf.clear();
        } else {
            self.out_buf.push(val);
        }
    }
    fn read(&mut self, _addr: u8) -> u8 {
        if self.in_buf.is_empty() {
            print!("STDIN requested: ");
            stdout().flush().expect("stdin failed");
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf).expect("stdin failed");
            self.in_buf.extend(buf.into_bytes());
        }
        self.in_buf
            .pop_front()
            .expect("in_buf has just been filled, this should be impossible")
    }
}

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Binary to debug
    #[arg(required = true)]
    input_file: PathBuf,

    /// Sets custom entry point for this to start execution at
    ///
    /// Can be either a hexadecimal address prefixed by 0x or a symbol
    #[arg(short = 'E', long)]
    entry: Option<String>,
}

fn main() -> ExitCode {
    let Cli { input_file, entry } = Cli::parse();

    let mut machine;
    let mut labels = HashMap::new();
    let mut pos_to_labels = HashMap::new();
    {
        let obj = match Object::from_file(&input_file) {
            Ok(o) => o,
            Err(e) => {
                eprintln!("could not read object file: {e}");

                return ExitCode::FAILURE;
            }
        };

        machine = Machine::new(LazyMain::new(DbgIo {
            in_buf: VecDeque::new(),
            out_buf: Vec::new(),
        }), Blf4::new());
        machine.load_user_binary(&obj);

        if let Some(entry) = entry {
            if let Some(entry) = entry.strip_prefix("0x") {
                let Ok(addr) = u16::from_str_radix(entry, 16) else {
                    eprintln!("could not parse entry point in hex format");
                    return ExitCode::FAILURE;
                };
                machine.cpu.program_counter = addr;
            } else {
                let mut ep_candidate = None;
                for &SymbolDefinition {
                    ref name,
                    is_global,
                    location,
                    ..
                } in &obj.symbols.0
                {
                    if **name == *entry {
                        if is_global {
                            ep_candidate = Some(location);
                            // break because global symbols are unique
                            break;
                        } else if ep_candidate.is_none() {
                            ep_candidate = Some(location);
                            // don't break in case a global symbol exists
                        }
                    }
                }

                if let Some(ep) = ep_candidate {
                    machine.cpu.program_counter = ep;
                }
            }
        }

        for SymbolDefinition {
            name,
            location,
            is_global,
            ..
        } in obj.symbols.into_iter()
        {
            if is_global {
                labels.insert(name.clone(), location);
                pos_to_labels.insert(location, name);
            } else {
                labels.entry(name.clone()).or_insert(location);
                pos_to_labels.entry(location).or_insert(name);
            }
        }
    }

    tdbg_loop(machine, pos_to_labels);

    ExitCode::SUCCESS
}

fn tdbg_loop(mut machine: Machine<LazyMain<DbgIo>, Blf4>, pos_to_labels: HashMap<u16, Box<str>>) {
    let stdin = stdin();
    let mut input = String::new();
    let mut target_nesting = 0;
    let mut current_nesting = 0;

    'disassemble_loop: loop {
        let dins = disassemble_instruction(&mut machine, |p| {
            pos_to_labels.get(&p).map(|s| &**s)
        }).unwrap();

        if false && todo!("is trapping") {
            println!("handled trap encountered!");
            current_nesting += 1;
            target_nesting = current_nesting;
        }

        let mut skip_cmd_loop = true;
        if current_nesting == target_nesting {
            if let Some(label) = pos_to_labels.get(&machine.cpu.program_counter) {
                println!("<{label}>:");
            }

            println!("{}", dins.annotated_source);

            if dins.ends_block || dins.nesting_difference != 0 {
                println!();
            }

            skip_cmd_loop = false;
        }

        let next_nesting = current_nesting + dins.nesting_difference;

        target_nesting = loop {
            if skip_cmd_loop {
                break target_nesting;
            }

            print!("+tdgb> ");
            stdout().flush().expect("stdout failed");

            input.clear();
            stdin.read_line(&mut input).expect("stdin failed");

            match input.trim() {
                "q" | "quit" => break 'disassemble_loop,
                "n" | "next" => {
                    break current_nesting;
                }
                "si" | "in" | "stepin" => {
                    break next_nesting;
                }
                "so" | "out" | "stepout" => {
                    break current_nesting - 1;
                }
                l if l.starts_with("r ") => {
                    let arg = l[2..].trim();
                    let addr = match parse_num(arg) {
                        Ok(addr) => addr,
                        Err(s) => {
                            eprintln!("{s}");
                            continue;
                        }
                    };
                    let mut c = machine.cpu.context(&mut machine.memory);
                    println!(
                        " = 0x{:02x} 0x{:02x} ...",
                        c.read(addr).unwrap(),
                        c.read(addr + 1).unwrap()
                    );
                }
                "r0b" => print_byte_register("r0b", R0B, &machine.cpu),
                "r1l" => print_byte_register("r1l", R1L, &machine.cpu),
                "r1h" => print_byte_register("r1h", R1H, &machine.cpu),
                "r2l" => print_byte_register("r2l", R2L, &machine.cpu),
                "r2h" => print_byte_register("r2h", R2H, &machine.cpu),
                "r3l" => print_byte_register("r3l", R3L, &machine.cpu),
                "r3h" => print_byte_register("r3h", R3H, &machine.cpu),
                "r4l" => print_byte_register("r4l", R4L, &machine.cpu),
                "r4h" => print_byte_register("r4h", R4H, &machine.cpu),
                "r5l" => print_byte_register("r5l", R5L, &machine.cpu),
                "r5h" => print_byte_register("r5h", R5H, &machine.cpu),
                "r6b" => print_byte_register("r6b", R6B, &machine.cpu),
                "r7b" => print_byte_register("r7b", R7B, &machine.cpu),
                "r8b" => print_byte_register("r8b", R8B, &machine.cpu),
                "r9b" => print_byte_register("r9b", R9B, &machine.cpu),
                "r10b" => print_byte_register("r10b", R10B, &machine.cpu),
                "r0" => println!("r0 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R0).unwrap()),
                "r1" => println!("r1 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R1).unwrap()),
                "r2" => println!("r2 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R2).unwrap()),
                "r3" => println!("r3 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R3).unwrap()),
                "r4" => println!("r4 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R4).unwrap()),
                "r5" => println!("r5 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R5).unwrap()),
                "r6" => println!("r6 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R6).unwrap()),
                "r7" => println!("r7 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R7).unwrap()),
                "r8" => println!("r8 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R8).unwrap()),
                "r9" => println!("r9 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R9).unwrap()),
                "r10" => println!("r10 = {r} 0x{r:04x}", r = machine.cpu.read_wr(R10).unwrap()),
                "rs" => println!("rs = {r} 0x{r:04x}", r = machine.cpu.stack),
                "rl" => println!("rl = {r} 0x{r:04x}", r = machine.cpu.link),
                "rf" => println!("rf = {r} 0x{r:04x}", r = machine.cpu.frame),
                "rp" => println!("rp = {r} 0x{r:04x}", r = machine.cpu.page),
                "rh" => println!("rh = {r} 0x{r:04x}", r = machine.cpu.trap_handler),
                "rpc" => println!("pc = {pc} 0x{pc:04x}", pc = machine.cpu.program_counter),
                "flags" => println!("flags = {}", machine.cpu.flags),
                l if l.starts_with("g ") => {
                    let arg = l[2..].trim();
                    let addr = match parse_num(arg) {
                        Ok(addr) => addr,
                        Err(s) => {
                            eprintln!("{s}");
                            continue;
                        }
                    };
                    machine.cpu.program_counter = addr;
                    continue 'disassemble_loop;
                }
                _ => eprintln!("unknown command, type q to quit"),
            }
        };

        match machine.execute_once() {
            Ok(()) => (),
            Err(e) => {
                println!("ended with {e:?}");
                break 'disassemble_loop;
            }
        }
        current_nesting = next_nesting;
    }
}

fn print_byte_register(name: &str, r: ByteRegister, reg: &Blf4) {
    let val = reg.read_br(r);
    print!("{name} = {val} 0x{val:02x}");
    if let Ok(s) = std::str::from_utf8(&[val]) {
        // TODO: don't rely on debug print here
        print!(" {s:?}");
    }
    println!();
}

fn parse_num(num: &str) -> Result<u16, &'static str> {
    Ok(if let Some(num) = num.strip_prefix("0x") {
        u16::from_str_radix(num, 16).map_err(|_| "invalid hex number")?
    } else if let Some(num) = num.strip_prefix("0o") {
        u16::from_str_radix(num, 8).map_err(|_| "invalid octal number")?
    } else if let Some(num) = num.strip_prefix("0b") {
        u16::from_str_radix(num, 2).map_err(|_| "invalid binary number")?
    } else {
        num.parse().map_err(|_| "invalid decimal number")?
    })
}
