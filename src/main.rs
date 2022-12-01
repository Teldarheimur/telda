use std::{fs::File, env::args, io::{BufReader, Write}, collections::HashMap};

use telda2::{source::{SourceLines, process, DataLine, DataOperand, WBigR, BReg, WReg, Wide, BBigR}, mem::Lazy, cpu::Cpu};

fn big_r_to_byte(br: BBigR) -> u8 {
    match br {
        BBigR::Register(r) => r as u8,
        BBigR::Byte(0) => BReg::Zero as u8,
        // Since this b is a number from 1 up to 247, we can just add 7 to encode it between 0x08 and 0xff
        BBigR::Byte(b) => b.checked_add(7).expect("immediate between 1-247"),
    }
}
fn big_r_to_wide(wr: WBigR, id_to_pos: &HashMap<usize, u16>) -> [u8; 2] {
    match wr {
        WBigR::Register(r) => r as u16,
        WBigR::Wide(w) => {
            let w = parse_wide(w, id_to_pos);
            if w == 0 {
                WReg::Zero as u16
            } else {
                // Since this w is a number from 1 up to 65527, we can just add 7 to encode it between 0x08 and 0xffff
                w.checked_add(7).expect("immediate between 1-247")
            }
        }
    }.to_le_bytes()
}

fn parse_wide(w: Wide, id_to_pos: &HashMap<usize, u16>) -> u16 {
    match w {
        Wide::Label(l) => *id_to_pos.get(&l).expect("no such label"),
        Wide::Number(n) => n,
    }
}

fn write_data_operand(mem: &mut Vec<u8>, id_to_pos: &HashMap<usize, u16>, dat_op: DataOperand) {
    use self::DataOperand::*;

    match dat_op {
        Nothing => (),
        ByteBigR(br) => mem.push(big_r_to_byte(br)),
        WideBigR(wr) => mem.extend_from_slice(&big_r_to_wide(wr, id_to_pos)),
        ByteRegister(r) => mem.push((r as u8) << 4),
        WideRegister(r) => mem.push((r as u8) << 4),
        ImmediateByte(b) => {
            mem.push(b);
        }
        ImmediateWide(w) => {
            mem.extend_from_slice(&parse_wide(w, id_to_pos).to_le_bytes());
        }
        TwoByteOneBig(r1, r2, br) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(big_r_to_byte(br));
        }
        WideBigByte(r1, wr, r2) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, id_to_pos));
        }
        ByteWideBig(r1, r2, wr) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, id_to_pos));
        }
        WideBigWide(r1, wr, r2) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, id_to_pos));
        }
        TwoWideOneBig(r1, r2, wr) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.extend_from_slice(&big_r_to_wide(wr, id_to_pos));
        }
        FourByte(r1, r2, r3, r4) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(((r3 as u8) << 4) | r4 as u8);
        }
        FourWide(r1, r2, r3, r4) => {
            mem.push(((r1 as u8) << 4) | r2 as u8);
            mem.push(((r3 as u8) << 4) | r4 as u8);
        }
    }
}

fn main() {
    for arg in args().skip(1) {
        let f = File::open(&arg).unwrap();
        let (id_to_pos, labels, data_lines) = process(SourceLines::new(BufReader::new(f)));
        let mut mem = Vec::with_capacity(256);
        for data_line in data_lines {
            match data_line {
                DataLine::Raw(mut bytes) => {
                    mem.append(&mut bytes);
                }
                DataLine::Ins(opcode, dat_op) => {
                    mem.push(opcode);

                    write_data_operand(&mut mem, &id_to_pos, dat_op);
                }
            }
        }

        let mut f = File::create(format!("{arg}.bin")).unwrap();
        f.write_all(&mem).unwrap();
        let mut f = File::create(format!("{arg}.symbols")).unwrap();
        for (&id, &loc) in id_to_pos.iter() {
            let lbl = &labels[id];
            writeln!(f, "{lbl}: 0x{loc:02X}").unwrap();
        }

        let start_id = labels.iter().position(|l| &**l == "_start").unwrap();
        let mut cpu = Cpu::new(id_to_pos[&start_id]);

        let tm = cpu.run_until_trap(&mut Lazy { mem });
        let pc = cpu.registers.pc;
        let mut diff = u16::MAX;
        let mut closest = start_id; // dummy value
        for (&id, &loc) in id_to_pos.iter() {
            if pc <= loc {
                let new_diff = loc - pc;
                if new_diff < diff {
                    diff = new_diff;
                    closest = id;
                }
            }
        }
        let closest = &labels[closest];
        println!("Ended with {tm:?} at <{closest}+{diff:02X}>");
    }
}