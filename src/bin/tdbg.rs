use std::{
    collections::{HashMap, VecDeque},
    io::{stdin, stdout, Write},
    path::PathBuf,
    process::ExitCode,
};

use telda2::{
    aalv::obj::{Object, SymbolDefinition},
    cpu::*,
    disassemble::disassemble_instruction,
    mem::{Io, Lazy, Memory},
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

    let mem;
    let ep;
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

        mem = obj.get_flattened_memory();

        if let Some(entry) = entry {
            if let Some(entry) = entry.strip_prefix("0x") {
                let Ok(addr) = u16::from_str_radix(entry, 16) else {
                    eprintln!("could not parse entry point in hex format");
                    return ExitCode::FAILURE;
                };
                ep = Some(addr);
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
                            break;
                        } else if ep_candidate.is_none() {
                            ep_candidate = Some(location);
                        }
                    }
                }
                ep = ep_candidate;
            }
        } else {
            ep = obj.entry.map(|e| e.1);
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

    let io = DbgIo {
        in_buf: VecDeque::new(),
        out_buf: Vec::new(),
    };
    let mut mem = Lazy { io, mem };
    let Some(start) = ep else {
        eprintln!("no _entry section in binary, cannot start");
        eprintln!("help: you can set a custom one with -E");
        return ExitCode::FAILURE;
    };
    let mut cpu = Cpu::new(start);
    let stdin = stdin();
    let mut input = String::new();
    let mut target_nesting = 0;
    let mut current_nesting = 0;

    'disassemble_loop: loop {
        let dins = disassemble_instruction(cpu.registers.program_counter, &mem.mem, |p| {
            pos_to_labels.get(&p).map(|s| &**s)
        });

        if cpu.registers.trap {
            println!("handled trap encountered!");
            current_nesting += 1;
            target_nesting = current_nesting;
        }

        let mut skip_loop = true;
        if current_nesting == target_nesting {
            if let Some(label) = pos_to_labels.get(&cpu.registers.program_counter) {
                println!("<{label}>:");
            }

            println!("{}", dins.annotated_source);

            if dins.ends_block || dins.nesting_difference != 0 {
                println!();
            }

            skip_loop = false;
        }

        let next_nesting = current_nesting + dins.nesting_difference;

        target_nesting = loop {
            if skip_loop {
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
                    println!(
                        " = 0x{:02x} 0x{:02x} ...",
                        mem.read(addr),
                        mem.read(addr + 1)
                    );
                }
                "r0b" => print_byte_register("r0b", R0B, &cpu.registers),
                "r1l" => print_byte_register("r1l", R1L, &cpu.registers),
                "r1h" => print_byte_register("r1h", R1H, &cpu.registers),
                "r2l" => print_byte_register("r2l", R2L, &cpu.registers),
                "r2h" => print_byte_register("r2h", R2H, &cpu.registers),
                "r3l" => print_byte_register("r3l", R3L, &cpu.registers),
                "r3h" => print_byte_register("r3h", R3H, &cpu.registers),
                "r4l" => print_byte_register("r4l", R4L, &cpu.registers),
                "r4h" => print_byte_register("r4h", R4H, &cpu.registers),
                "r5l" => print_byte_register("r5l", R5L, &cpu.registers),
                "r5h" => print_byte_register("r5h", R5H, &cpu.registers),
                "r6b" => print_byte_register("r6b", R6B, &cpu.registers),
                "r7b" => print_byte_register("r7b", R7B, &cpu.registers),
                "r8b" => print_byte_register("r8b", R8B, &cpu.registers),
                "r9b" => print_byte_register("r9b", R9B, &cpu.registers),
                "r10b" => print_byte_register("r10b", R10B, &cpu.registers),
                "r0" => println!("r0 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R0)),
                "r1" => println!("r1 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R1)),
                "r2" => println!("r2 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R2)),
                "r3" => println!("r3 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R3)),
                "r4" => println!("r4 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R4)),
                "r5" => println!("r5 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R5)),
                "r6" => println!("r6 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R6)),
                "r7" => println!("r7 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R7)),
                "r8" => println!("r8 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R8)),
                "r9" => println!("r9 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R9)),
                "r10" => println!("r10 = {r} 0x{r:04x}", r = cpu.registers.read_wide(R10)),
                "rs" => println!("rs = {r} 0x{r:04x}", r = cpu.registers.read_wide(RS)),
                "rl" => println!("rl = {r} 0x{r:04x}", r = cpu.registers.read_wide(RL)),
                "rf" => println!("rf = {r} 0x{r:04x}", r = cpu.registers.read_wide(RF)),
                "rp" => println!("rp = {r} 0x{r:04x}", r = cpu.registers.read_wide(RP)),
                "rh" => println!("rh = {r} 0x{r:04x}", r = cpu.registers.read_wide(RH)),
                "rpc" => println!("pc = {pc} 0x{pc:04x}", pc = cpu.registers.program_counter),
                "flags" => {
                    print!("flags = ");
                    if cpu.registers.carry {
                        print!("carry ");
                    }
                    if cpu.registers.overflow {
                        print!("overflow ");
                    }
                    if cpu.registers.sign {
                        print!("sign ");
                    }
                    if cpu.registers.zero {
                        print!("zero ");
                    }
                    println!();
                }
                l if l.starts_with("g ") => {
                    let arg = l[2..].trim();
                    let addr = match parse_num(arg) {
                        Ok(addr) => addr,
                        Err(s) => {
                            eprintln!("{s}");
                            continue;
                        }
                    };
                    cpu.registers.program_counter = addr;
                    continue 'disassemble_loop;
                }
                _ => eprintln!("unknown command, type q to quit"),
            }
        };
        match cpu.run_instruction(&mut mem) {
            Ok(()) => (),
            Err(e) => {
                println!("ended with {e:?}");
                break 'disassemble_loop;
            }
        }
        current_nesting = next_nesting;
    }

    ExitCode::SUCCESS
}

fn print_byte_register(name: &str, r: ByteRegister, reg: &Registers) {
    let val = reg.read_byte(r);
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
