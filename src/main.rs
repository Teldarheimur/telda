use std::{fs::File, env::args, io::{BufReader, Write}, collections::HashMap};

use telda2::{source::{SourceLines, process, DataLine, DataOperand, BigR, Reg, Wide}, mem::Lazy, cpu::Cpu};

fn big_r_to_byte(br: BigR) -> u8 {
    match br {
        BigR::Register(r) => r as u8,
        BigR::Byte(0) => Reg::Zero as u8,
        // Since this b is a number from 1 up to 247, we can just add 7 to encode it between 0x08 and 0xff
        BigR::Byte(b) => b.checked_add(7).expect("immediate between 1-247"),
    }
}

fn write_data_operand(mem: &mut Vec<u8>, labels: &HashMap<String, u16>, dat_op: DataOperand) {
    use self::DataOperand::*;

    match dat_op {
        Nothing => (),
        BigR(br) => mem.push(big_r_to_byte(br)),
        Register(r) => mem.push((r as u8) << 4),
        TwoRegistersAndBigR(r1, r2, br) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(big_r_to_byte(br));
        }
        ImmediateByte(b) => {
            mem.push(b);
        }
        ImmediateWide(w) => {
            let arr = match w {
                Wide::Label(l) => *labels.get(&*l).expect("no such label"),
                Wide::Number(n) => n,
            }.to_le_bytes();
            mem.extend_from_slice(&arr);
        }
        Hr(h, r) => {
            let [low, high] = h.to_le_bytes();
            mem.extend_from_slice(&[low, (high << 4) | (r as u8)]);
        }
        Rh(r, h) => {
            let [low, high] = h.to_le_bytes();
            let high = (low >> 4) | (high << 4);
            mem.extend_from_slice(&[((r as u8) << 4) | (low & 0x0f), high]);
        }
        FourRegisters(r1, r2, r3, r4) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(((r3 as u8) << 4) | r4 as u8);
        }
        ThreeRegisters(r1, r2, r3) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push((r3 as u8) << 4);
        }
        BRegisters(r1, r2, r3) => {
            mem.push(((r1 as u8) << 6) | ((r2 as u8) << 4) | r3 as u8);
        }
    }
}

fn main() {
    for arg in args().skip(1) {
        let f = File::open(&arg).unwrap();
        let (labels, data_lines) = process(SourceLines::new(BufReader::new(f)));
        let mut mem = Vec::with_capacity(256);
        for data_line in data_lines {
            match data_line {
                DataLine::Raw(mut bytes) => {
                    mem.append(&mut bytes);
                }
                DataLine::Ins(opcode, dat_op) => {
                    mem.push(opcode);

                    write_data_operand(&mut mem, &labels, dat_op);
                }
            }
        }

        let mut f = File::create(format!("{arg}.bin")).unwrap();
        f.write_all(&mem).unwrap();
        let mut f = File::create(format!("{arg}.symbols")).unwrap();
        for (lbl, &loc) in labels.iter() {
            writeln!(f, "{lbl}: 0x{loc:02X}").unwrap();
        }

        let mut cpu = Cpu::new(labels["_start"]);

        let tm = cpu.run_until_trap(&mut Lazy { mem });
        let pc = cpu.registers.pc;
        let mut diff = u16::MAX;
        let mut closest = "";
        for (lbl, &loc) in labels.iter() {
            if pc <= loc {
                let new_diff = loc - pc;
                if new_diff < diff {
                    diff = new_diff;
                    closest = lbl;
                }
            }
        }
        println!("Ended with {tm:?} at <{closest}+{diff:02X}>");
    }
}