use std::{env::args, io::{stdin, stdout, Write}, collections::{HashMap, VecDeque}, path::Path};

use telda2::{mem::{Memory, Lazy, Io}, cpu::*, disassemble::disassemble_instruction, aalv::obj::{Object, SymbolDefinition}};

struct DbgIo {
    in_buf: VecDeque<u8>,
    out_buf: Vec<u8>,
}

impl Io for DbgIo {
    fn write(&mut self, _addr: u8, val: u8) {
        if val == b'\n' {
            print!("STDOUT line: ");
            std::io::stdout().write_all(&self.out_buf).unwrap();
            println!();
            self.out_buf.clear();
        } else {
            self.out_buf.push(val);
        }
    }
    fn read(&mut self, _addr: u8) -> u8 {
        if self.in_buf.is_empty() {
            print!("STDIN requested: ");
            stdout().flush().unwrap();
            let mut buf = String::new();
            std::io::stdin().read_line(&mut buf).unwrap();
            self.in_buf.extend(buf.into_bytes());
        }
        self.in_buf.pop_front().unwrap()
    }
}

fn main() {
    let arg = args().nth(1).unwrap();
    let p = Path::new(&arg);

    let mem;
    let ep;
    let mut labels = HashMap::new();
    let mut pos_to_labels = HashMap::new();
    {
        let obj = Object::from_file(p).unwrap();

        mem = obj.get_flattened_memory();

        ep = obj.entry.map(|e| e.0);

        for SymbolDefinition{name, location, is_global, ..} in obj.symbols.into_iter() {
            if is_global {
                labels.insert(name.clone(), location);
                pos_to_labels.insert(location, name);
            } else {
                labels.entry(name.clone()).or_insert(location);
                pos_to_labels.entry(location).or_insert(name);
            }
        }
    }

    let io = DbgIo { in_buf: VecDeque::new(), out_buf: Vec::new() };
    let mut mem = Lazy { io, mem };
    let start = ep.unwrap_or_else(|| {eprintln!("warning: no _start segment in binary, using 0 as startpoint"); 0});
    let mut cpu = Cpu::new(start);
    let stdin = stdin();
    let mut input = String::new();
    let mut target_nesting = 0;
    let mut current_nesting = 0;

    'disassemble_loop: loop {
        let dins = disassemble_instruction(cpu.registers.program_counter, &mem.mem, |p| pos_to_labels.get(&p).map(|s| &**s));

        if current_nesting == target_nesting {
            if let Some(label) = pos_to_labels.get(&cpu.registers.program_counter) {
                println!("<{label}>:");
            }

            println!("{}", dins.annotated_source);

            if dins.ends_block || dins.nesting_difference != 0 {
                println!();
            }
        }

        let next_nesting = current_nesting + dins.nesting_difference;

        target_nesting = loop {
            if target_nesting != current_nesting {
                break target_nesting;
            }

            print!("+tdgb> ");
            stdout().flush().unwrap();

            input.clear();
            stdin.read_line(&mut input).unwrap();

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
                    let addr = if arg.starts_with("0x") {
                        u16::from_str_radix(&arg[2..], 16).unwrap()
                    } else {
                        arg.parse().unwrap()
                    };
                    println!(" = 0x{:02x} 0x{:02x} ...", mem.read(addr), mem.read(addr+1));
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
                    let addr = if arg.starts_with("0x") {
                        u16::from_str_radix(&arg[2..], 16).unwrap()
                    } else {
                        arg.parse().unwrap()
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
