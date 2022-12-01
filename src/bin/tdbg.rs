use std::{fs::File, env::args, io::{BufReader, Read, BufRead, stdin}, collections::HashMap, path::Path};

use telda2::{mem::{Lazy, Memory}, cpu::{Cpu, ByteRegister, Registers}, disassemble::disassemble_instruction};

fn main() {
    let arg = args().nth(1).unwrap();
    let p = Path::new(&arg);
    let mut mem = Vec::new();
    let mut f = File::open(p).unwrap();
    f.read_to_end(&mut mem).unwrap();
    let mut labels = HashMap::new();
    let mut pos_to_labels = HashMap::new();
    let f = File::open(p.with_extension("tsym")).unwrap();
    for line in BufReader::new(f).lines() {
        let line = line.unwrap();
        let colon = line.find(':').unwrap();
        let lbl = line[..colon].to_owned();
        let pos = u16::from_str_radix(&line[colon+4..], 16).unwrap();
        labels.insert(
            lbl.clone(),
            pos
        );
        pos_to_labels.insert(
            pos,
            lbl
        );
    }

    let start_id = labels.get("_start").copied()
        .unwrap_or_else(|| {eprintln!("warning: no _start symbol found, using as 0 startpoint"); 0});
    let mut mem = Lazy { mem };
    let mut cpu = Cpu::new(start_id);
    let mut stdin = stdin().lock();
    let mut input = String::new();
    let mut continuing = false;

    'disassemble_loop: loop {
        let mut new_labels = Vec::new();

        if let Some(label) = pos_to_labels.get(&cpu.registers.pc) {
            println!("<{label}>:");
        }

        let dins = disassemble_instruction(cpu.registers.pc, &mem.mem, &mut new_labels, &pos_to_labels);
        println!("{}", dins.annotated_source);
        if !dins.does_not_end_function {
            continuing = false;
        }

        'command_loop: loop {
            if continuing { break }

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
