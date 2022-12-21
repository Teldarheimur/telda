use std::{env::args, io::{stdin, stdout, Write}, collections::{HashMap, VecDeque}, path::Path};

use telda2::{mem::{Memory}, cpu::{Cpu, ByteRegister, Registers, IoPort}, disassemble::disassemble_instruction, aalv::obj::Object};

struct DbgIoPort {
    in_buf: VecDeque<u8>,
    out_buf: Vec<u8>,
}

impl IoPort for DbgIoPort {
    fn write(&mut self, val: u8) {
        if val == b'\n' {
            print!("STDOUT line: ");
            std::io::stdout().write_all(&self.out_buf).unwrap();
            println!();
            self.out_buf.clear();
        } else {
            self.out_buf.push(val);
        }
    }
    fn read(&mut self) -> u8 {
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

    let mut mem;
    let mut labels = HashMap::new();
    let mut pos_to_labels = HashMap::new();
    {
        let obj = Object::from_file(p).unwrap();
        mem = obj.mem.unwrap();

        let iter = obj.internal_symbols
            .map(|is| is.0.into_iter())
            .into_iter()
            .flatten()
            .chain(obj.global_symbols.map(|is| is.0.into_iter()).into_iter().flatten());

        for (label, position) in iter {
            labels.insert(label.clone(), position);
            pos_to_labels.insert(position, label);
        }
    }

    let io_port = DbgIoPort { in_buf: VecDeque::new(), out_buf: Vec::new() };
    let start_id = labels.get("_start").copied()
        .unwrap_or_else(|| {eprintln!("warning: no _start symbol found, using as 0 startpoint"); 0});
    let mut cpu = Cpu::new(start_id, Box::new(io_port));
    let stdin = stdin();
    let mut input = String::new();
    let mut target_nesting = 0;
    let mut current_nesting = 0;

    'disassemble_loop: loop {
        let dins = disassemble_instruction(cpu.registers.pc, &mem.mem, |p| pos_to_labels.get(&p).map(|s| &**s));

        if current_nesting == target_nesting {
            if let Some(label) = pos_to_labels.get(&cpu.registers.pc) {
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
                "ra" => println!("a = {a} 0x{a:04x}", a = cpu.registers.a),
                "rb" => println!("b = {b} 0x{b:04x}", b = cpu.registers.b),
                "rc" => println!("c = {c} 0x{c:04x}", c = cpu.registers.c),
                "rx" => println!("x = {x} 0x{x:04x}", x = cpu.registers.x),
                "ry" => println!("y = {y} 0x{y:04x}", y = cpu.registers.y),
                "rz" => println!("z = {z} 0x{z:04x}", z = cpu.registers.z),
                "rsp" | "rs" => println!("sp = {sp} 0x{sp:04x}", sp = cpu.registers.sp),
                "rpc" | "rp" => println!("pc = {pc} 0x{pc:04x}", pc = cpu.registers.pc),
                "r0" => println!("zero is zero"),
                "ral" => print_byte_register("al", 1, &mut cpu.registers),
                "rah" => print_byte_register("ah", 2, &mut cpu.registers),
                "rbl" => print_byte_register("bl", 3, &mut cpu.registers),
                "rbh" => print_byte_register("bh", 4, &mut cpu.registers),
                "rcl" => print_byte_register("cl", 5, &mut cpu.registers),
                "rch" => print_byte_register("ch", 6, &mut cpu.registers),
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
                    cpu.registers.pc = addr;
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

fn print_byte_register(name: &str, r: u8, reg: &mut Registers) {
    let val = reg.read_byte(ByteRegister::new(r));
    print!("{name} = {val} 0x{val:02x}");
    if let Ok(s) = std::str::from_utf8(&[val]) {
        // TODO: don't rely on debug print here
        print!(" {s:?}");
    }
    println!();
}
