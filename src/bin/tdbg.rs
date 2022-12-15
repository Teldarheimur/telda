use std::{env::args, io::{BufRead, stdin, stdout, Write}, collections::HashMap, path::Path};

use telda2::{mem::{Memory}, cpu::{Cpu, ByteRegister, Registers}, disassemble::disassemble_instruction, aalv::obj::ShebangAgnosticObject};

fn main() {
    let arg = args().nth(1).unwrap();
    let p = Path::new(&arg);

    let mut mem;
    let mut labels = HashMap::new();
    let mut pos_to_labels = HashMap::new();
    {
        let obj = ShebangAgnosticObject::from_file(p).unwrap().into_object();
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

    let start_id = labels.get("_start").copied()
        .unwrap_or_else(|| {eprintln!("warning: no _start symbol found, using as 0 startpoint"); 0});
    let mut cpu = Cpu::new(start_id);
    let mut stdin = stdin().lock();
    let mut input = String::new();
    let mut continuing = false;

    'disassemble_loop: loop {
        if let Some(label) = pos_to_labels.get(&cpu.registers.pc) {
            println!("<{label}>:");
        }

        let dins = disassemble_instruction(cpu.registers.pc, &mem.mem, |p| pos_to_labels.get(&p).map(|s| &**s));
        println!("{}", dins.annotated_source);
        if !dins.does_not_end_function {
            continuing = false;
        }

        'command_loop: loop {
            if continuing { break }

            print!("+tdgb> ");
            stdout().flush().unwrap();

            input.clear();
            stdin.read_line(&mut input).unwrap();

            match input.trim() {
                "q" | "quit" => break 'disassemble_loop,
                "" | "s" | "step" => break 'command_loop,
                "c" | "continue" => {
                    continuing = true;
                    break 'command_loop;
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
                "ral" => print_byte_register("al", 1, &cpu.registers),
                "rah" => print_byte_register("ah", 2, &cpu.registers),
                "rbl" => print_byte_register("bl", 3, &cpu.registers),
                "rbh" => print_byte_register("bh", 4, &cpu.registers),
                "rcl" => print_byte_register("cl", 5, &cpu.registers),
                "rch" => print_byte_register("ch", 6, &cpu.registers),
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
        }
        match cpu.run_instruction(&mut mem) {
            Ok(()) => (),
            Err(e) => {
                println!("ended with {e:?}");
                break 'disassemble_loop;
            }
        }
    }
}

fn print_byte_register(name: &str, r: u8, reg: &Registers) {
    let val = reg.read_byte(ByteRegister::new(r));
    print!("{name} = {val} 0x{val:02x}");
    if let Ok(s) = std::str::from_utf8(&[val]) {
        // TODO: don't rely on debug print here
        print!(" {s:?}");
    }
    println!();
}
