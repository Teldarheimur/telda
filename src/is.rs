use crate::{TeldaEndian, Memory, MemoryIter, NextIndex};
use std::iter::Iterator;
use byteorder::ByteOrder;

macro_rules! instructions {
    ($enum_name:ident; $($name:ident $(,$sname:expr)*; $opcode:expr;)*) => {
        $(
            pub const $name: u8 = $opcode;
        )*
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
        #[repr(u8)]
        pub enum $enum_name {
            $(
                $name = $name
            ),*
        }

        // This is here to make sure all opcodes are defined
        fn _dummy(n: u8) {
            match n {
                0b0100_0000..=0xff => (),
                $($opcode => ()),*
            }
        }

        impl $enum_name {
            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $(stringify!($name) $(| $sname)* => Some(Self::$name),)*
                    _ => None,
                }
            }
            pub fn from_u8(i: u8) -> Option<Self> {
                if i & 0xc0 == 0 {
                    unsafe {
                        Some(std::mem::transmute(i))
                    }
                } else {
                    None
                }
            }
        }
    };
}

// OPCODE
// llgg iiii
// l: length of args 
// g: instruction group
// i: instruction

// TODO: Remove `l` here, not all instrcuctions have variants with all 4 possibilities, this'll free up a few opcodes for new instrcuctions
// This'll require re-working `Opcode` and likely `Args` too, but it might end up neater
// This'll mess up all old binaries too, since all opcodes will be shifted

instructions!{Opcode;
    INVALID; 0x00;
    LOAD, "ld", "load"; 0x01;
    DEPLDL; 0x02;
    STR, "str", "store"; 0x03;
    DEPSTL; 0x04;
    COMPARE, "cmp"; 0x05;
    ADD, "inc", "add"; 0x0a;
    SUB, "dec", "sub"; 0x0b;
    MUL, "mul"; 0x0c;
    DIV, "div"; 0x0d;
    REM, "rem"; 0x0e;
    NOP, "nop"; 0x0f;
    AND, "and"; 0x06;
    OR, "or"; 0x07;
    XOR, "xor"; 0x08;
    NOT, "not"; 0x09;
    JUMP, "jmp", "jump"; 0x10;
    JMPR, "jmpr"; 0x18;
    // RGLZ (all = overflow)
    // relative greater less zero
    JEZ, "jez"; 0x11;
    JEZR, "jezr"; 0x19;
    JLT, "jlt"; 0x12;
    JLTR, "jltr"; 0x1a;
    JLE, "jle"; 0x13;
    JLER, "jler"; 0x1b;
    JGT, "jgt"; 0x14;
    JGTR, "jgtr"; 0x1c;
    JGE, "jge"; 0x15;
    JGER, "jger"; 0x1d;
    JNE, "jne"; 0x16;
    JNER, "jner"; 0x1e;
    JIO, "jio"; 0x17;
    JIOR, "jior"; 0x1f;

    PUSH, "psh", "push"; 0x20;
    PUSHW, "pshw", "pushw"; 0x21;
    POP, "pop"; 0x22;
    POPW, "popw"; 0x23;
    CALL, "call"; 0x24;
    RET, "ret"; 0x25;
    
    RES6; 0x26;
    RES7; 0x27;
    RES8; 0x28;
    RES9; 0x29;
    RESA; 0x2a;
    RESB; 0x2b;
    RESC; 0x2c;
    RESD; 0x2d;
    RESE; 0x2e;
    RESF; 0x2f;

    HALT, "int0", "halt"; 0x30;
    INT1, "int1"; 0x31;
    INT2, "int2"; 0x32;
    INT3, "int3"; 0x33;
    INT4, "int4"; 0x34;
    INT5, "int5"; 0x35;
    INT6, "int6"; 0x36;
    INT7, "int7"; 0x37;
    INT8, "int8"; 0x38;
    INT9, "int9"; 0x39;
    INT10, "inta", "int10"; 0x3a;
    INT11, "intb", "int11"; 0x3b;
    INT12, "intc", "int12"; 0x3c;
    INT13, "intd", "int13"; 0x3d;
    INT14, "inte", "int14"; 0x3e;
    INT15, "intf", "int15"; 0x3f;
}

/*

    Reg(Reg), // $name 110?_rrRR (? = 1 if RR)
    Ref(Reference), // [$reg(+o)] 0rrs_oooo | [val] 1110_00rr wide | [$reg(+o)] (o>0xf) 1001_rrRR iwide
    OffsetReg(Reg, i16), // $reg+o 1111_rrRR iwide
    Byte(u8), // bbb 1000_00rr byte
    Wide(u16), // dddddd(w) 1010_00rr wide

*/

pub trait IntoI16 {
    fn into_i16(n: u16) -> i16;
}

impl IntoI16 for u16 {
    #[inline(always)]
    fn into_i16(n: u16) -> i16 {
        n as i16
    }
}
impl IntoI16 for u8 {
    #[inline(always)]
    fn into_i16(n: u16) -> i16 {
        n as u8 as i8 as i16
    }
}

pub fn read_instruction<'a, T: 'a + Memory<I>, I: 'a + IntoI16 + Into<u16>>(mut bytes: MemoryIter<'a, T, I>) -> (Opcode, Args, usize)
where MemoryIter<'a, T, I>: Iterator<Item=u8> + NextIndex<I>, usize: From<I> {
    fn read_wide<'a, T: 'a + Memory<I>, I: 'a + Into<u16> + Into<usize>>(len: &mut usize, bytes: &mut MemoryIter<'a, T, I>) -> u16
    where MemoryIter<'a, T, I>: Iterator<Item=u8> + NextIndex<I>, usize: From<I>  {
        *len += usize::from(<T as Memory<I>>::INDEX_WIDTH);
        bytes.next_index().into()
    }

    let mut len = 1;
    let op = bytes.next().unwrap();

    let args = (op & 0b1100_0000) >> 6;
    let opcode = Opcode::from_u8(op & 0b0011_1111).unwrap();

    let args = match args {
        0b00 => Args { fst: None, snd: None },
        0b01 => {
            len += 1;
            let reg = Reg::from_u8(bytes.next().unwrap() & 0b11);
            
            Args { fst: Some(reg), snd: None }
        },
        0b10 => Args { fst: None, snd: Some({
            len += 1;
            let arg = bytes.next().unwrap();

            if arg & 0b1000_0000 == 0 {
                let offset_sign = if (arg & 0b1_0000) == 0 { 0 } else { 0b1111_0000 };

                FullArg::Ref(Reference::Reg{ reg: Reg::from_u8((arg & 0b0110_0000) >> 5), offset: (offset_sign | (arg & 0b1111)) as i8 as i16})
            } else if arg & 0b1100_0000 == 0b1000_0000 {
                match arg & 0b0011_0000 {
                    0b0000_0000 => {
                        len += 1;
                        FullArg::Byte(bytes.next().unwrap())
                    }
                    0b0010_0000 => FullArg::Wide(read_wide(&mut len, &mut bytes)),
                    0b0001_0000 => {
                        // [$reg(+o)] (o>0xf) 1001_rrRR iwide

                        let offset = I::into_i16(read_wide(&mut len, &mut bytes));

                        FullArg::Ref(Reference::Reg{reg: Reg::from_u8((arg & 0b0000_1100) >> 2), offset})
                    }
                    0b0011_0000 => unimplemented!("Not used yet. On an old version?"),
                    _ => unreachable!(),
                }
            } else if arg & 0b1110_0000 == 0b1100_0000 {
                FullArg::Reg(Reg::from_u8((arg & 0b0000_1100) >> 2))
            } else if arg & 0b1111_0000 == 0b1110_0000 {
                FullArg::Ref(Reference::Val(read_wide(&mut len, &mut bytes)))
            } else {
                assert_eq!(arg & 0b1111_0000, 0b1111_0000, "Invalid args");

                let offset = I::into_i16(read_wide(&mut len, &mut bytes));

                FullArg::OffsetReg(Reg::from_u8((arg & 0b0000_1100) >> 2), offset)
            }
        }) },
        0b11 => {
            len += 1;
            let arg = bytes.next().unwrap();

            let (fst, snd);

            if arg & 0b1000_0000 == 0b0000_0000 {
                len += 1;
                let offset_sign = if (arg & 0b1_0000) == 0 { 0 } else { 0b1111_0000 };
                snd = FullArg::Ref(Reference::Reg{ reg: Reg::from_u8((arg & 0b0110_0000) >> 5), offset: (offset_sign | (arg & 0b1111)) as i8 as i16});
                fst = Reg::from_u8(bytes.next().unwrap() & 0b0000_0011);
            } else if arg & 0b1100_0000 == 0b1000_0000 {
                snd = match arg & 0b0011_0000 {
                    0b0000_0000 => {
                        len += 1;
                        FullArg::Byte(bytes.next().unwrap())
                    }
                    0b0010_0000 => FullArg::Wide(read_wide(&mut len, &mut bytes)),
                    0b0001_0000 => {
                        // [$reg(+o)] (o>0xf) 1001_rrRR iwide

                        let offset = I::into_i16(read_wide(&mut len, &mut bytes));

                        FullArg::Ref(Reference::Reg{reg: Reg::from_u8((arg & 0b0000_1100) >> 2), offset})
                    }
                    0b0011_0000 => unimplemented!("Not used yet, using a new binary?"),
                    _ => unreachable!(),
                };

                fst = Reg::from_u8(arg & 0b0000_0011);
            } else if arg & 0b1110_0000 == 0b1100_0000 {
                fst = Reg::from_u8(arg & 0b0000_0011);
                snd = FullArg::Reg(Reg::from_u8((arg & 0b0000_1100) >> 2));
            } else if arg & 0b1111_0000 == 0b1110_0000 {
                fst = Reg::from_u8(arg & 0b0000_0011);
                snd = FullArg::Ref(Reference::Val(read_wide(&mut len, &mut bytes)));
            } else {
                assert_eq!(arg & 0b1111_0000, 0b1111_0000, "Invalid args");
                fst = Reg::from_u8(arg & 0b0000_0011);

                let offset = I::into_i16(read_wide(&mut len, &mut bytes));

                snd = FullArg::OffsetReg(Reg::from_u8((arg & 0b0000_1100) >> 2), offset);
            }

            Args{
                fst: Some(fst),
                snd: Some(snd),
            }
        },
        _ => unreachable!(),
    };

    (opcode, args, len)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Args {
    pub fst: Option<Reg>,
    pub snd: Option<FullArg>,
}

impl Args {
    pub fn mask(&self) -> u8 {
        (self.fst.is_some() as u8 * 0b0100_0000) | (self.snd.is_some() as u8 * 0b1000_0000)
    }
    pub fn write(&self, mut bytes: &mut [u8]) -> usize {
        let bit8_mode = false;
        let mut len = 0;

        let write_wide: Box<dyn Fn(&mut usize, u16, &mut [u8])> = if bit8_mode {
            Box::new(|len, w, bytes| {
                *len += 1;
                bytes[0] = w as u8;
            })
        } else {
            Box::new(|len, w, bytes| {
                *len += 2;
                TeldaEndian::write_u16(bytes, w);
            })
        };

        match self {
            Args{fst: None, snd: None} => (),
            Args{fst: Some(r), snd: None} => {
                bytes[0] = *r as u8;
                len += 1;
            },
            Args{fst: None, snd: Some(f)} => {
                match f {
                    FullArg::Ref(Reference::Reg { reg : r, offset: o}) => {
                        debug_assert!(if bit8_mode {
                            *o as i8 as i16 == *o
                        } else { true }, "impossible offset");

                        if (*o & 0b01111111_11111111) < 0x10 {
                            let o = *o as i8 as u8;
    
                            len += 1;
                            bytes[0] = ((*r as u8) << 5) | ((o & 0b1000_0000) >> 3) | ((o & 0b1111) as u8);
                        } else {
                            len += 1;
                            bytes[0] = 0b1001_0000 | ((*r as u8) << 2);
                            write_wide(&mut len, *o as u16, &mut bytes[1..]);
                        }
                    }
                    FullArg::OffsetReg(reg, o) => {
                        len += 1;
                        bytes[0] = 0b1111_0000 | ((*reg as u8) << 2);
                        write_wide(&mut len, *o as u16, &mut bytes[1..]);
                    }
                    FullArg::Byte(b) => {
                        len += 2;
                        bytes[0] = 0b1000_0000;
                        bytes[1] = *b;  
                    }
                    FullArg::Wide(w) => {
                        len += 1;
                        bytes[0] = 0b1010_0000;
                        write_wide(&mut len, *w, &mut bytes[1..]);
                    }
                    FullArg::Reg(r) => {
                        len += 1;
                        bytes[0] = 0b1100_0000 | ((*r as u8) << 2);
                    }
                    FullArg::Ref(Reference::Val(w)) => {
                        len += 1;
                        bytes[0] = 0b1110_0000;
                        write_wide(&mut len, *w, &mut bytes[1..]);
                    }
                }
            },
            Args{fst: Some(f_reg), snd: Some(f)} => {
                match f {
                    FullArg::Ref(Reference::Reg { reg : r, offset: o}) => {
                        debug_assert!(if bit8_mode {
                            *o as i8 as i16 == *o
                        } else { true }, "impossible offset");
                        if (*o & 0b01111111_11111111) < 0x10 {
                            let o = *o as u8;

                            len += 2;
                            bytes[0] = ((*r as u8) << 5) | ((o & 0b1000_0000) >> 3) | ((o & 0b1111) as u8);
                            bytes[1] = *f_reg as u8;
                        } else {
                            len += 1;
                            bytes[0] = 0b1001_0000 | ((*r as u8) << 2) | *f_reg as u8;
                            write_wide(&mut len, *o as u16, &mut bytes[1..]);
                        }
                    }
                    FullArg::OffsetReg(r, o) => {
                        len += 1;
                        bytes[0] = 0b1111_0000 | ((*r as u8) << 2) | *f_reg as u8;
                        write_wide(&mut len, *o as u16, &mut bytes[1..]);
                    }
                    FullArg::Byte(b) => {
                        len += 2;
                        bytes[0] = 0b1000_0000 | *f_reg as u8;
                        bytes[1] = *b;  
                    }
                    FullArg::Wide(w) => {
                        len += 1;
                        bytes[0] = 0b1010_0000 | *f_reg as u8;
                        bytes = &mut bytes[1..];
                        write_wide(&mut len, *w, &mut bytes);
                    }
                    FullArg::Reg(r) => {
                        len += 1;
                        bytes[0] = 0b1101_0000 | ((*r as u8) << 2) | *f_reg as u8;
                    }
                    FullArg::Ref(Reference::Val(w)) => {
                        len += 1;
                        bytes[0] = 0b1110_0000 | *f_reg as u8;
                        bytes = &mut bytes[1..];
                        write_wide(&mut len, *w, &mut bytes);
                    }
                }
            },
        }

        len
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Memory<u16> for [u8; 4] {
        const INDEX_WIDTH: u16 = 2;
        fn read(&self, i: u16) -> u8 {
            self[i as usize]
        }
        fn read_index(&self, i: u16) -> u16 {
            TeldaEndian::read_u16(&self[i as usize..])
        }
        fn write(&mut self, i: u16, c: u8) {
            self[i as usize] = c;
        }
        fn write_index(&mut self, i: u16, c: u16) {
            TeldaEndian::write_u16(&mut self[i as usize..], c);
        }
        fn size(&self) -> usize { self.len() }
    }

    fn test_read_write(opcode: Opcode, args: Args) {
        let mut bytes = [opcode as u8 | args.mask(), 0, 0, 0];

        let len = args.write(&mut bytes[1..]);
        eprint!("{:02X?}", &bytes[..len+1]);
        eprintln!(" ++ {:02X?}", &bytes[len+1..]);
        let (oc, a, l) = read_instruction(bytes.read_iter_from(0u16));

        assert_eq!((len, opcode, args), (l-1, oc, a));
    }

    #[test]
    fn load_none_none() {
        test_read_write(Opcode::LOAD, Args {fst: None, snd: None});
    }
    #[test]
    fn load_none_wide() {
        test_read_write(Opcode::LOAD, Args {fst: None, snd: Some(FullArg::Wide(0xdead))});
    }
    #[test]
    fn load_zero() {
        test_read_write(Opcode::RET, Args {fst: None, snd: Some(FullArg::Wide(0x0000))});
    }
    #[test]
    fn load_none_byte() {
        test_read_write(Opcode::LOAD, Args {fst: None, snd: Some(FullArg::Byte(0xad))});
    }
    #[test]
    fn load_none_accw() {
        test_read_write(Opcode::LOAD, Args {fst: None, snd: Some(FullArg::Reg(Reg::AccW))});
    }
    #[test]
    fn load_none_acc() {
        test_read_write(Opcode::LOAD, Args {fst: None, snd: Some(FullArg::Reg(Reg::Acc))});
    }
    #[test]
    fn int2_ref_accw() {
        test_read_write(Opcode::INT2, Args { fst: None, snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::AccW, offset: -2}))});
    }
    #[test]
    fn store_reg_ref_accw() {
        test_read_write(Opcode::STR, Args { fst: Some(Reg::AccW), snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Sp, offset: -2}))});
    }
    #[test]
    fn load_reg_ref_accw_neg_big() {
        test_read_write(Opcode::STR, Args { fst: Some(Reg::AccW), snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: -100}))});
    }
    #[test]
    fn load_reg_ref_accw_pos_big() {
        test_read_write(Opcode::LOAD, Args { fst: Some(Reg::AccW), snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: 120}))});
    }
    #[test]
    fn load_ref_accw_neg_big() {
        test_read_write(Opcode::STR, Args { fst: None, snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: -100}))});
    }
    #[test]
    fn load_ref_accw_pos_big() {
        test_read_write(Opcode::LOAD, Args { fst: None, snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: 120}))});
    }
    #[test]
    fn load_into_bc_spp1() {
        test_read_write(Opcode::LOAD, Args { fst: Some(Reg::Bc), snd: Some(FullArg::Ref(Reference::Reg{reg: Reg::Sp, offset: 1}))});
    }
    #[test]
    fn push_offset_reg() {
        test_read_write(Opcode::PUSH, Args { fst: None, snd: Some(FullArg::OffsetReg(Reg::Acc, 120))});
    }
    #[test]
    fn push_reg_offset_reg() {
        test_read_write(Opcode::PUSH, Args { fst: Some(Reg::Sp), snd: Some(FullArg::OffsetReg(Reg::Acc, 120))});
    }
    #[test]
    fn push_offset_neg_reg() {
        test_read_write(Opcode::PUSH, Args { fst: None, snd: Some(FullArg::OffsetReg(Reg::Acc, -120))});
    }
    #[test]
    fn push_reg_offset_neg_reg() {
        test_read_write(Opcode::PUSH, Args { fst: Some(Reg::Sp), snd: Some(FullArg::OffsetReg(Reg::Acc, -120))});
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Reg {
    Sp = 0b00,
    Bc = 0b01,
    Acc = 0b10,
    AccW = 0b11,
    // Flags = 0b100,
    // Bp = 0b101,
    // BpW = 0b110,
}

impl Reg {
    #[inline(always)]
    fn from_u8(b: u8) -> Reg {
        debug_assert!(b < 0b100);
        unsafe { std::mem::transmute(b) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Reference {
    Reg{reg: Reg, offset: i16},
    Val(u16),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FullArg {
    // RR is the register argument of the argument pair
    Reg(Reg), // $name 110?_rrRR (? = 1 if RR)
    Ref(Reference), // [$reg(+o)] 0rrs_oooo | [val] 1110_00RR wide | [$reg(+o)] (o>0xf) 1001_rrRR iwide
    OffsetReg(Reg, i16), // $reg+o 1111_rrRR iwide
    Byte(u8), // bbb 1000_00RR byte
    Wide(u16), // dddddd(w) 1010_00RR wide

    // Unassigned:
    /*
    1011_xxxx
    *(y != 0):
    1000_yyxx
    1010_yyxx
    */
}
