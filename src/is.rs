use crate::{TeldaEndian, Memory, MemoryIter, NextIndex};
use std::iter::Iterator;
use std::fmt::{self, Debug};
use byteorder::ByteOrder;

macro_rules! instructions {
    ($enum_name:ident; $uncovered:pat =>  $invalid:ident => $invalid_opcode:expr; $($(#[$attrib:meta])* $name:ident $(,$sname:expr)*; $arg_ident:tt => $opcode:expr;)*) => {
        pub const $invalid: u8 = $invalid_opcode;
        $(
            $(#[$attrib])*
            pub const $name: u8 = $opcode;
        )*
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
        #[repr(u8)]
        pub enum $enum_name {
            $invalid = $invalid,
            $(
                $(#[$attrib])*
                $name = $name
            ),*
        }

        // This is here to make sure all opcodes are defined
        fn _dummy(n: u8) {
            match n {
                $invalid_opcode => (),
                $uncovered => (),
                $($opcode => ()),*
            }
        }

        impl $enum_name {
            pub fn from_str(s: &str, arg_identity: ArgIdentity) -> Self {
                match (s, arg_identity) {
                    $( (stringify!($name), a!($arg_ident)) $(| ($sname, a!($arg_ident)) )* => Self::$name,)*
                    _ => Self::$invalid,
                }
            }
            #[track_caller]
            pub fn into_str(self) -> &'static str {
                match self {
                    Self::$invalid => panic!("the invalid opcode has no str representation"),
                    $( Self::$name => instructions!(@NAMES $($sname;)* stringify!($name);), )*
                }
            }
            /// Panics on the invalid opcode
            #[track_caller]
            pub fn arg_identity(self) -> ArgIdentity {
                match self {
                    Self::$invalid => panic!("the invalid opcode has no arg identity"),
                    $( Self::$name => a!($arg_ident), )*
                }
            }
            pub fn from_u8(i: u8) -> Self {
                match i {
                    $uncovered => Self::$invalid,
                    _ => unsafe { std::mem::transmute(i) },
                }
            }
            pub fn valid(self) -> Option<Self> {
                match self {
                    Self::$invalid => None,
                    valid => Some(valid)
                }
            }
        }
    };
    (@NAMES $main_name:expr; $($sname:expr;)*) => {
        $main_name
    };
}

#[derive(Debug, Copy, Clone)]
pub struct ArgIdentity {
    pub takes_reg: bool,
    pub takes_full: bool,
}
macro_rules! a {
    (DOC 00) => {
        "Takes no arguments"
    };
    (00) => {
        ArgIdentity {
            takes_reg: false,
            takes_full: false,
        }
    };
    (DOC 01) => {
        "Takes a registry value"
    };
    (01) => {
        ArgIdentity {
            takes_reg: true,
            takes_full: false,
        }
    };
    (DOC 10) => {
        "Takes a full argument"
    };
    (10) => {
        ArgIdentity {
            takes_reg: false,
            takes_full: true,
        }
    };
    (DOC 11) => {
        "Takes both a registry and a full argument"
    };
    (11) => {
        ArgIdentity {
            takes_reg: true,
            takes_full: true,
        }
    };
}
pub type FstArg = Reg;
pub type SndArg = FullArg;

instructions!{Opcode; 0x50..=0xff =>
    INVALID => 0x00;
    /// Load the given value into the A register
    LOADA, "load", "loada"; 10 => 0x01;
    /// Load the given value into the given register
    LOADAT, "load"; 11 => 0x02;
    /// Write a value from register A to the given address in memory
    STRA, "str"; 10 => 0x03;
    /// Write a value from the given register to the given address in memory
    STRAT, "str"; 11 => 0x04;
    /// Set the flags from comparing the A register with 0
    COMPARE0, "cmp"; 00 => 0x05;
    /// Set the flags from comparing the A register with the given value
    COMPAREA, "cmp"; 10 => 0x06;
    /// Set the flags from comparing the given register with the given value
    COMPAREAT, "cmp"; 11 => 0x07;

    ANDA, "and"; 10 => 0x08;
    ANDAT, "and"; 11 => 0x09;
    ORA, "or"; 10 => 0x0a;
    ORAT, "or"; 11 => 0x0b;
    XORA, "xor"; 10 => 0x0c;
    XORAT, "xor"; 11 => 0x0d;
    NOTA, "not"; 10 => 0x0e;
    NOTAT, "not"; 11 => 0x0f;

    INCA, "inc", "add"; 00 => 0x10;
    INCAT, "inc", "add"; 01 => 0x11;
    ADDA, "add"; 10 => 0x12;
    ADDAT, "add"; 11 => 0x13;
    DECA, "dec", "sub"; 00 => 0x14;
    DECAT, "dec", "sub"; 01 => 0x15;
    SUBA, "sub"; 10 => 0x16;
    SUBAT, "sub"; 11 => 0x17;
    MULA, "mul"; 10 => 0x18;
    MULAT, "mul"; 11 => 0x19;
    DIVA, "div"; 10 => 0x1a;
    DIVAT, "div"; 11 => 0x1b;
    REMA, "rem"; 10 => 0x1c;
    REMAT, "rem"; 11 => 0x1d;

    /// Reserverd arithmetic instruction
    RESARITE; 10 => 0x1e;
    /// Reserverd arithmetic instruction
    RESARITF; 11 => 0x1f;

    /// Group 2 reserved instruction 0
    RES0; 00 => 0x20;
    /// Group 2 reserved instruction 1
    RES1; 00 => 0x21;
    /// Group 2 reserved instruction 2
    RES2; 00 => 0x22;
    /// Group 2 reserved instruction 3
    RES3; 00 => 0x23;
    /// Group 2 reserved instruction 4
    RES4; 00 => 0x24;
    /// Sets the direction flag to 0, making `smv` decrement
    SDN, "sdn"; 00 => 0x25;
    /// Sets the direction flag to 1, making `smv` increment
    SDP, "sdp"; 00 => 0x26;
    /// String move
    /// 
    /// Writes [$sr] into [$ds] incrementing/decrementing the registers according to flags
    /// 
    /// Equivalent to doing sld $ab then sst $ab, but without actually using a register
    SMV, "smv"; 00 => 0x27;
    /// String store
    /// 
    /// Stores the given value into [$ds], and then increments/decrements $ds according to flags
    SST, "sst"; 10 => 0x28;
    /// String load
    /// 
    /// Loads the value from [$sr], and then increments/decrements $ds according to flags
    SLD, "sld"; 01 => 0x29;
    /// Pushes given value to the stack
    ///
    /// This decrements the stack register according to the size of the value
    /// and then writes the value to the memory location described by the stack register.
    PUSH, "push"; 10 => 0x2a;
    /// Pops a value the stack into the given register.
    ///
    /// This reads a value of the size of the given register
    /// at the memory address described by the stack register
    /// and then increments the stack register accordingly
    POP, "pop"; 01 => 0x2b;
    /// Call
    CALL, "call"; 10 => 0x2c;
    /// Return
    RET, "ret"; 00 => 0x2d;
    /// Store at stack offset
    /// 
    /// Writes to [$sp+val] from given register
    SAS, "sas"; 11 => 0x2e;
    /// No operation
    NOP, "nop"; 00 => 0x2f;

    // RGLZ (all = overflow)
    // relative greater less zero
    /// Jump to address in given value
    JUMP, "jmp", "jump"; 10 => 0x30;
    /// Jump to address plus given value interpreted as signed
    JMPR, "jmpr"; 10 => 0x38;

    /// Jump to address in given value, if the zero flag is set
    JEZ, "jez"; 10 => 0x31;
    /// Jump to address plus given value interpreted as signed, if the zero flag is set
    JEZR, "jezr"; 10 => 0x39;
    /// Jump to address in given value, if the less-than flag is set
    JLT, "jlt"; 10 => 0x32;
    /// Jump to address plus given value interpreted as signed, if the less-than flag is set
    JLTR, "jltr"; 10 => 0x3a;
    /// Jump to address in given value, if the less-than or equals flag is set
    JLE, "jle"; 10 => 0x33;
    /// Jump to address plus given value interpreted as signed, if the less-than or equals flag is set
    JLER, "jler"; 10 => 0x3b;
    /// Jump to address in given value, if the greater-than flag is set
    JGT, "jgt"; 10 => 0x34;
    /// Jump to address plus given value interpreted as signed, if the greater-than flag is set
    JGTR, "jgtr"; 10 => 0x3c;
    /// Jump to address in given value, if the greater-than or equals flag is set
    JGE, "jge"; 10 => 0x35;
    /// Jump to address plus given value interpreted as signed, if the greater-than or equals flag is set
    JGER, "jger"; 10 => 0x3d;
    /// Jump to address in given value, if either the greater-than or less-than is set (not the zero flag)
    JNE, "jne", "jnz"; 10 => 0x36;
    /// Jump to address plus given value interpreted as signed, if either the greater-than or less-than is set (not the zero flag)
    JNER, "jner"; 10 => 0x3e;
    /// Jump to address in given value, if the overflow flag is set.
    JIO, "jio"; 10 => 0x37;
    /// Jump to address plus given value interpreted as signed, if the overflow flag is set.
    JIOR, "jior"; 10 => 0x3f;

    /// Interrupt 0, more commonly known as halt, marks the end of a program
    HALT, "halt", "int0"; 00 => 0x40;
    /// Interrupt 1, commonly means read from STDIN
    INT1, "int1"; 00 => 0x41;
    /// Interrupt 2, commonly means write to STDOUT
    INT2, "int2"; 00 => 0x42;
    /// Interrupt 3, commonly means write to STDERR
    INT3, "int3"; 00 => 0x43;
    /// Interrupt 4
    INT4, "int4"; 00 => 0x44;
    /// Interrupt 5
    INT5, "int5"; 00 => 0x45;
    /// Interrupt 6
    INT6, "int6"; 00 => 0x46;
    /// Interrupt 7
    INT7, "int7"; 00 => 0x47;
    /// Interrupt 8
    INT8, "int8"; 00 => 0x48;
    /// Interrupt 9
    INT9, "int9"; 00 => 0x49;
    /// Interrupt 10
    INT10, "inta", "int10"; 00 => 0x4a;
    /// Interrupt 11
    INT11, "intb", "int11"; 00 => 0x4b;
    /// Interrupt 12
    INT12, "intc", "int12"; 00 => 0x4c;
    /// Interrupt 13
    INT13, "intd", "int13"; 00 => 0x4d;
    /// Interrupt 14
    INT14, "inte", "int14"; 00 => 0x4e;
    /// Interrupt 15
    INT15, "intf", "int15"; 00 => 0x4f;
}

pub trait InstructionHandler {
    type Fst;
    type Snd;
    type InterruptSignal;

    fn convert_fst(&self, fst: FstArg) -> Self::Fst;
    fn convert_snd(&self, snd: SndArg) -> Self::Snd;

    fn load(&mut self, reg: Self::Fst, val: Self::Snd);
    fn str(&mut self, reg: Self::Fst, location: Self::Snd);
    fn cmp(&mut self, reg: Self::Fst, rhs: Self::Snd);

    fn and(&mut self, reg: Self::Fst, val: Self::Snd);
    fn or(&mut self, reg: Self::Fst, val: Self::Snd);
    fn xor(&mut self, reg: Self::Fst, val: Self::Snd);
    fn not(&mut self, reg: Self::Fst, val: Self::Snd);
    
    fn inc(&mut self, reg: Self::Fst);
    fn dec(&mut self, reg: Self::Fst);
    fn add(&mut self, reg: Self::Fst, val: Self::Snd);
    fn sub(&mut self, reg: Self::Fst, val: Self::Snd);
    fn mul(&mut self, reg: Self::Fst, val: Self::Snd);
    fn div(&mut self, reg: Self::Fst, val: Self::Snd);
    fn rem(&mut self, reg: Self::Fst, val: Self::Snd);

    fn push(&mut self, snd: Self::Snd);
    fn pop(&mut self, reg: Self::Fst);
    fn store_at_stack_offset(&mut self, reg: Self::Fst, val: Self::Snd);
    fn setd(&mut self, direction: bool);
    fn sload(&mut self, reg: Self::Fst);
    fn sstore(&mut self, val: Self::Snd);
    fn smv(&mut self);
    fn call(&mut self, location: Self::Snd);
    fn ret(&mut self);

    fn jump(&mut self, location: Self::Snd);
    fn jumpr(&mut self, location: Self::Snd);
    fn jez(&mut self, location: Self::Snd);
    fn jezr(&mut self, location: Self::Snd);
    fn jlt(&mut self, location: Self::Snd);
    fn jltr(&mut self, location: Self::Snd);
    fn jle(&mut self, location: Self::Snd);
    fn jler(&mut self, location: Self::Snd);
    fn jgt(&mut self, location: Self::Snd);
    fn jgtr(&mut self, location: Self::Snd);
    fn jge(&mut self, location: Self::Snd);
    fn jger(&mut self, location: Self::Snd);
    fn jne(&mut self, location: Self::Snd);
    fn jner(&mut self, location: Self::Snd);
    fn jio(&mut self, location: Self::Snd);
    fn jior(&mut self, location: Self::Snd);

    fn int0(&mut self) -> Option<Self::InterruptSignal>;
    fn int1(&mut self) -> Option<Self::InterruptSignal>;
    fn int2(&mut self) -> Option<Self::InterruptSignal>;
    fn int3(&mut self) -> Option<Self::InterruptSignal>;
    fn int4(&mut self) -> Option<Self::InterruptSignal>;
    fn int5(&mut self) -> Option<Self::InterruptSignal>;
    fn int6(&mut self) -> Option<Self::InterruptSignal>;
    fn int7(&mut self) -> Option<Self::InterruptSignal>;
    fn int8(&mut self) -> Option<Self::InterruptSignal>;
    fn int9(&mut self) -> Option<Self::InterruptSignal>;
    fn int10(&mut self) -> Option<Self::InterruptSignal>;
    fn int11(&mut self) -> Option<Self::InterruptSignal>;
    fn int12(&mut self) -> Option<Self::InterruptSignal>;
    fn int13(&mut self) -> Option<Self::InterruptSignal>;
    fn int14(&mut self) -> Option<Self::InterruptSignal>;
    fn int15(&mut self) -> Option<Self::InterruptSignal>;
}

pub fn handle<T: InstructionHandler>(h: &mut T, op_and_arg: OpAndArg) -> Option<T::InterruptSignal> {
    use Opcode::*;


    #[cfg(feature = "print_instruction")]
    {
        let s = op_and_arg.map(String::new, |r| format!("{:?}", r), |s| format!("{:?}", s), |f, s| format!("{:?}, {:?}", f, s));
    
        eprintln!("{:?} {}", op_and_arg.opcode, s);
    
        drop(s);
        // std::thread::sleep_ms(500);
    }

    match op_and_arg.opcode {
        NOP => (),
        RES0 | RES1 | RES2 | RES3 | RES4 | RESARITE | RESARITF => panic!("RESERVED"),
        INVALID => panic!("Invalid instruction call!"),
        LOADA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.load(h.convert_fst(reg), h.convert_snd(snd));
        }
        LOADAT => {
            let (fst, snd) = op_and_arg.both();

            h.load(h.convert_fst(fst), h.convert_snd(snd));
        }
        STRA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.str(h.convert_fst(reg), h.convert_snd(snd));
        }
        STRAT => {
            let (fst, snd) = op_and_arg.both();

            h.str(h.convert_fst(fst), h.convert_snd(snd));
        }
        COMPARE0 => {
            h.cmp(h.convert_fst(Reg::new(false)), h.convert_snd(FullArg::Byte(0)));
        }
        COMPAREA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.cmp(h.convert_fst(reg), h.convert_snd(snd));
        }
        COMPAREAT => {
            let (fst, snd) = op_and_arg.both();

            h.cmp(h.convert_fst(fst), h.convert_snd(snd));
        }
        DECA => {
            op_and_arg.none();
            h.dec(h.convert_fst(Reg::Ac));
        }
        DECAT => {
            let fst = op_and_arg.fst();

            h.dec(h.convert_fst(fst));
        }
        INCA => {
            op_and_arg.none();
            h.inc(h.convert_fst(Reg::Ac));
        }
        INCAT => {
            let fst = op_and_arg.fst();

            h.inc(h.convert_fst(fst));
        }
        SUBA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.sub(h.convert_fst(reg), h.convert_snd(snd));
        }
        SUBAT => {
            let (fst, snd) = op_and_arg.both();

            h.sub(h.convert_fst(fst), h.convert_snd(snd));
        }
        ADDA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.add(h.convert_fst(reg), h.convert_snd(snd));
        }
        ADDAT => {
            let (fst, snd) = op_and_arg.both();

            h.add(h.convert_fst(fst), h.convert_snd(snd));
        }
        MULA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.mul(h.convert_fst(reg), h.convert_snd(snd));
        }
        MULAT => {
            let (fst, snd) = op_and_arg.both();

            h.mul(h.convert_fst(fst), h.convert_snd(snd));
        }
        DIVA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.div(h.convert_fst(reg), h.convert_snd(snd));
        }
        DIVAT => {
            let (fst, snd) = op_and_arg.both();

            h.div(h.convert_fst(fst), h.convert_snd(snd));
        }
        REMA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.rem(h.convert_fst(reg), h.convert_snd(snd));
        }
        REMAT => {
            let (fst, snd) = op_and_arg.both();

            h.rem(h.convert_fst(fst), h.convert_snd(snd));
        }

        ANDA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.and(h.convert_fst(reg), h.convert_snd(snd));
        }
        ANDAT => {
            let (fst, snd) = op_and_arg.both();

            h.and(h.convert_fst(fst), h.convert_snd(snd));
        }
        ORA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.or(h.convert_fst(reg), h.convert_snd(snd));
        }
        ORAT => {
            let (fst, snd) = op_and_arg.both();

            h.or(h.convert_fst(fst), h.convert_snd(snd));
        }
        XORA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.xor(h.convert_fst(reg), h.convert_snd(snd));
        }
        XORAT => {
            let (fst, snd) = op_and_arg.both();

            h.xor(h.convert_fst(fst), h.convert_snd(snd));
        }
        NOTA => {
            let snd = op_and_arg.snd();
            let reg = Reg::new(snd.is_wide().unwrap_or_else(|b| b));

            h.not(h.convert_fst(reg), h.convert_snd(snd));
        }
        NOTAT => {
            let (fst, snd) = op_and_arg.both();

            h.not(h.convert_fst(fst), h.convert_snd(snd));
        }

        JUMP => {
            h.jump(h.convert_snd(op_and_arg.snd()));
        }
        JMPR => {
            h.jumpr(h.convert_snd(op_and_arg.snd()));
        }
        JEZ => h.jez(h.convert_snd(op_and_arg.snd())),
        JEZR => h.jezr(h.convert_snd(op_and_arg.snd())),
        JLT => h.jlt(h.convert_snd(op_and_arg.snd())),
        JLTR => h.jltr(h.convert_snd(op_and_arg.snd())),
        JLE => h.jle(h.convert_snd(op_and_arg.snd())),
        JLER => h.jler(h.convert_snd(op_and_arg.snd())),
        JGT => h.jgt(h.convert_snd(op_and_arg.snd())),
        JGTR => h.jgtr(h.convert_snd(op_and_arg.snd())),
        JGE => h.jge(h.convert_snd(op_and_arg.snd())),
        JGER => h.jger(h.convert_snd(op_and_arg.snd())),
        JNE => h.jne(h.convert_snd(op_and_arg.snd())),
        JNER => h.jner(h.convert_snd(op_and_arg.snd())),
        JIO => h.jio(h.convert_snd(op_and_arg.snd())),
        JIOR => h.jior(h.convert_snd(op_and_arg.snd())),
        PUSH => h.push(h.convert_snd(op_and_arg.snd())),
        POP => h.pop(h.convert_fst(op_and_arg.fst())),
        SAS => {
            let (fst, snd) = op_and_arg.both();
            h.store_at_stack_offset(h.convert_fst(fst), h.convert_snd(snd));
        }

        SDP => {
            op_and_arg.none();
            h.setd(true);
        }
        SDN => {
            op_and_arg.none();
            h.setd(false);
        }
        SLD => h.sload(h.convert_fst(op_and_arg.fst())),
        SST => h.sstore(h.convert_snd(op_and_arg.snd())),
        SMV => {
            op_and_arg.none();
            h.smv();
        }
        RET => {
            op_and_arg.none();
            h.ret();
        }
        CALL => h.call(h.convert_snd(op_and_arg.snd())),

        HALT => {
            op_and_arg.none();
            return h.int0();
        }
        INT1 => {
            op_and_arg.none();
            return h.int1();
        }
        INT2 => {
            op_and_arg.none();
            return h.int2();
        }
        INT3 => {
            op_and_arg.none();
            return h.int3();
        }
        INT4 => {
            op_and_arg.none();
            return h.int4();
        }
        INT5 => {
            op_and_arg.none();
            return h.int5();
        }
        INT6 => {
            op_and_arg.none();
            return h.int6();
        }
        INT7 => {
            op_and_arg.none();
            return h.int7();
        }
        INT8 => {
            op_and_arg.none();
            return h.int8();
        }
        INT9 => {
            op_and_arg.none();
            return h.int9();
        }
        INT10 => {
            op_and_arg.none();
            return h.int10();
        }
        INT11 => {
            op_and_arg.none();
            return h.int11();
        }
        INT12 => {
            op_and_arg.none();
            return h.int12();
        }
        INT13 => {
            op_and_arg.none();
            return h.int13();
        }
        INT14 => {
            op_and_arg.none();
            return h.int14();
        }
        INT15 => {
            op_and_arg.none();
            return h.int15();
        }
    }
    None
}

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

#[derive(Clone, Copy)]
union Args {
    no: (),
    fst: FstArg,
    snd: SndArg,
    both: (FstArg, SndArg),
}

#[derive(Clone, Copy)]
pub struct OpAndArg {
    pub opcode: Opcode,
    args: Args,
}

impl Debug for OpAndArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("OpAndArg")
            .field("opcode", &self.opcode)
            .field("args", unsafe { match self.opcode.arg_identity() {
                a!(00) => &self.args.no,
                a!(01) => &self.args.fst,
                a!(10) => &self.args.snd,
                a!(11) => &self.args.both,
            } })
            .finish()
    }
}

impl PartialEq for OpAndArg {
    fn eq(&self, other: &Self) -> bool {
        self.opcode == other.opcode &&
        unsafe { match self.opcode.arg_identity() {
            a!(00) => true,
            a!(01) => self.args.fst == other.args.fst,
            a!(10) => self.args.snd == other.args.snd,
            a!(11) => self.args.both == other.args.both,
        } }
    }
}

impl OpAndArg {
    pub fn from_str(s: &str, args: (Option<FstArg>, Option<SndArg>)) -> Option<Self> {
        Some(match args {
            (None, None) => OpAndArg {
                opcode: Opcode::from_str(s, a!(00)).valid()?,
                args: Args { no: () }
            },
            (Some(fst), None) => OpAndArg {
                opcode: Opcode::from_str(s, a!(01)).valid()?,
                args: Args { fst }
            },
            (None, Some(snd)) => OpAndArg {
                opcode: Opcode::from_str(s, a!(10)).valid()?,
                args: Args { snd }
            },
            (Some(fst), Some(snd)) => {
                if snd.is_wide().map(|b| b != fst.is_wide()).unwrap_or(false)  {
                    return None;
                }

                OpAndArg {
                    opcode: Opcode::from_str(s, a!(11)).valid()?,
                    args: Args { both: (fst, snd) }
                }
            },
        })
    }

    pub fn new_no(opcode: Opcode) -> Option<Self> {
        if matches!(opcode.arg_identity(), a!(00)) {
            Some(OpAndArg {
                opcode,
                args: Args {
                    no: ()
                }
            })
        } else {
            None
        }
    }
    pub fn new_fst(opcode: Opcode, fst: FstArg) -> Option<Self> {
        if matches!(opcode.arg_identity(), a!(01)) {
            Some(OpAndArg {
                opcode,
                args: Args {
                    fst
                }
            })
        } else {
            None
        }
    }
    pub fn new_snd(opcode: Opcode, snd: SndArg) -> Option<Self> {
        if matches!(opcode.arg_identity(), a!(10)) {
            Some(OpAndArg {
                opcode,
                args: Args {
                    snd
                }
            })
        } else {
            None
        }
    }
    pub fn new_both(opcode: Opcode, both: (FstArg, SndArg)) -> Option<Self> {
        if matches!(opcode.arg_identity(), a!(11)) {
            Some(OpAndArg {
                opcode,
                args: Args {
                    both
                }
            })
        } else {
            None
        }
    }

    pub fn map<R, F0: FnOnce() -> R, F1: FnOnce(FstArg) -> R, F2: FnOnce(SndArg) -> R, F3: FnOnce(FstArg, SndArg) -> R>(self, f0: F0, f1: F1, f2: F2, f3: F3) -> R {
        unsafe { match self.opcode.arg_identity() {
            a!(00) => f0(),
            a!(01) => f1(self.args.fst),
            a!(10) => f2(self.args.snd),
            a!(11) => {
                let Args { both: (fst, snd) } = self.args;
                f3(fst, snd)
            }
        }}
    }

    pub fn get_fst(self) -> Option<FstArg> {
        unsafe { match self.opcode.arg_identity() {
            a!(01) => Some(self.args.fst),
            a!(11) => Some(self.args.both.0),
            a!(00) | a!(10) => None,
        }}
    }
    pub fn get_snd(self) -> Option<SndArg> {
        unsafe { match self.opcode.arg_identity() {
            a!(10) => Some(self.args.snd),
            a!(11) => Some(self.args.both.1),
            a!(00) | a!(01) => None,
        }}
    }

    #[inline(always)]
    pub fn none(self) -> () {
        debug_assert!(matches!(self.opcode.arg_identity(), a!(00)), "not a no-arg op");
    }
    #[inline(always)]
    pub fn fst(self) -> FstArg {
        debug_assert!(matches!(self.opcode.arg_identity(), a!(01)), "not a fst op");
        unsafe { self.args.fst }
    }
    #[inline(always)]
    pub fn snd(self) -> SndArg {
        debug_assert!(matches!(self.opcode.arg_identity(), a!(10)), "not a snd op");
        unsafe { self.args.snd }
    }
    #[inline(always)]
    pub fn both(self) -> (FstArg, SndArg) {
        debug_assert!(matches!(self.opcode.arg_identity(), a!(11)), "not a both op");
        unsafe { self.args.both }
    }
}

/// Converts a number from the format xsoo_oooo
/// to the number it represents. The x can be any value
/// and is ignored.
/// 
/// `s` is 1 if it's negative, 0 if it's positive
/// 
/// `oo_oooo` is read as an unigned number with one added to it
/// 
/// ## Returns
/// 
/// (+/-) (`oo_oooo` + 1) (in a range from -64 to and including +64, but excluding 0)
const fn convert_offset(raw_offset: u8) -> i8 {
    let signed = raw_offset & 0b0100_0000 != 0;
    let uoffset = (raw_offset & 0b0011_1111) + 1;

    if signed {
        -(uoffset as i8)
    } else {
        uoffset as i8
    }
}

/// Converts an offset into a raw offset, see [`convert_offset`]
/// 
/// ## Returns
/// 
/// None if given 0, otherwise Some(raw_offset)
/// 
/// ## Note
/// 
/// If the value is out of the range ([-64, 64]), then it will either
/// panic (with debug assertions) or have an undefined return value
fn to_raw_offset(offset: i8) -> Option<u8> {
    if offset == 0 {
        return None;
    }
    Some({
        let signed = offset.is_negative();
        let abs_minus_one = offset.abs() as u8 - 1;

        debug_assert_eq!(abs_minus_one & 0b1100_0000, 0, "offset {} is out of range and became ({}, {}={2:08b})", offset, signed, abs_minus_one);

        ((signed as u8) << 6) | abs_minus_one
    })
}

#[test]
fn raw_offset_eq_convert_offset() {
    for offset in -64..=64 {
        match to_raw_offset(offset) {
            None => assert_eq!(offset, 0),
            Some(raw_offset) => assert_eq!(convert_offset(raw_offset), offset)
        }
    }
}

#[track_caller]
pub fn read_instruction<'a, T: 'a + Memory<I>, I: 'a + IntoI16 + Into<u16>>(mut bytes: MemoryIter<'a, T, I>) -> (OpAndArg, usize)
where MemoryIter<'a, T, I>: Iterator<Item=u8> + NextIndex<I>, usize: From<I> {
    fn read_wide<'a, T: 'a + Memory<I>, I: 'a + Into<u16> + Into<usize>>(len: &mut usize, bytes: &mut MemoryIter<'a, T, I>) -> u16
    where MemoryIter<'a, T, I>: Iterator<Item=u8> + NextIndex<I>, usize: From<I>  {
        *len += usize::from(<T as Memory<I>>::INDEX_WIDTH);
        bytes.next_index().into()
    }

    let mut len = 1;
    let op = bytes.next().unwrap();

    let opcode = if let Some(opcode) = Opcode::from_u8(op).valid() {
        opcode
    } else {
        panic!("invalid opcode {:02x} at {:02x}", op, bytes.index.into().wrapping_sub(1))
    };
    let args = opcode.arg_identity();

    let args = match args {
        a!(00) => Args { no: () },
        a!(01) => {
            len += 1;
            // The last 4 bits can be whatever
            let b = bytes.next().unwrap() & 0b1111_0000;
            // The fourth bit has to be zero
            debug_assert_eq!(b & 0b0001_0000, 0);
            let reg = Reg::from_u8(b >> 5);

            Args { fst: reg }
        },
        a!(10) => Args { snd: {
            len += 1;
            let arg = bytes.next().unwrap();

            // First bit zero? it's a register
            if arg & 0b1000_0000 == 0 {
                // The last four bits can be whatever
                // The first bit is zero, so it's already what we need it to be
                FullArg::Reg(Reg::from_u8(arg >> 4))
            // The leading two bits are 10? it's an immediate value
            } else if arg & 0b1100_0000 == 0b1000_0000 {
                // The bit after the leading 10 marks whether it's a wide
                // The rest can be whatever
                let wide = arg & 0b0010_0000 != 0;
                if wide {
                    FullArg::Wide(read_wide(&mut len, &mut bytes))
                } else {
                    len += 1;
                    FullArg::Byte(bytes.next().unwrap())
                }
            // The leading two bits are 11, it's a reference:
            } else {
                let immediate = arg & 0b0010_0000 == 0;
                let wide = arg & 0b0001_0000 != 0;
                // if the next bit is zero, it's an immediate reference
                if immediate {
                    let location = read_wide(&mut len, &mut bytes);

                    FullArg::ImmRef(wide, location)
                // Otherwise it's a reference from register
                } else {
                    let has_offset = arg & 0b0000_1000 != 0;
                    let offset = if has_offset {
                        len += 1;
                        // We ignore the very first bit
                        convert_offset(bytes.next().unwrap())
                    } else {
                        0
                    };

                    FullArg::RegRef(wide, Reg::from_u8(arg & 0b0111), offset)
                }
            }
        } },
        a!(11) => {
            len += 1;
            let arg = bytes.next().unwrap();

            let fst = Reg::from_u8(arg >> 5);
            let arg1 = arg & 0b0001_0000 != 0;
            let arg2 = arg & 0b0000_1000 != 0;

            let snd = match (arg1, arg2) {
                // Has a second register (if reference, no offset)
                (true, is_ref) => {
                    let reg2 = Reg::from_u8(arg & 0b0111);
                    if is_ref {
                        FullArg::RegRef(fst.is_wide(), reg2, 0)
                    } else {
                        FullArg::Reg(reg2)
                    }
                }
                // Immediate value
                (false, false) => {
                    if fst.is_wide() {
                        let imm = read_wide(&mut len, &mut bytes);
                        FullArg::Wide(imm)
                    } else {
                        len += 1;
                        let imm = bytes.next().unwrap();
                        FullArg::Byte(imm)
                    }
                }
                // Reference that's either immediate or from a register with offset
                (false, true) => {
                    let from_register = arg & 0b0100 != 0;

                    if from_register {
                        let reg2 = (arg & 0b0011) << 1;
                        len += 1;
                        let r_and_offset = bytes.next().unwrap();
                        let reg2 = Reg::from_u8(reg2 | ((r_and_offset & 0b1000_0000) >> 7));

                        FullArg::RegRef(fst.is_wide(), reg2, convert_offset(r_and_offset))
                    } else {
                        let loc = read_wide(&mut len, &mut bytes);
                        FullArg::ImmRef(fst.is_wide(), loc)
                    }
                }
            };

            Args{
                both: (fst, snd)
            }
        },
    };

    (OpAndArg{opcode, args}, len)
}

#[inline]
fn write_wide(len: &mut usize, w: u16, bytes: &mut [u8]) {
    *len += 2;
    TeldaEndian::write_u16(bytes, w);
}

pub fn write_snd(bytes: &mut [u8], snd: SndArg) -> usize {
    let mut len = 0;
    
    match snd {
        FullArg::Reg(reg) => {
            len += 1;
            bytes[0] = (reg as u8) << 4;
        }
        FullArg::Byte(b) => {
            len += 2;
            //           10wx_xxxx
            bytes[0] = 0b1000_0000;
            bytes[1] = b;
        }
        FullArg::Wide(w) => {
            len += 1;
            //           10wx_xxxx
            bytes[0] = 0b1010_0000;
            write_wide(&mut len, w, &mut bytes[1..]);
        }
        FullArg::ImmRef(w, imm) => {
            len += 1;
            //           110w_xxxx
            bytes[0] = 0b1100_0000 | ((w as u8) << 4);
            write_wide(&mut len, imm, &mut bytes[1..]);
        }
        FullArg::RegRef(w, reg, offset) => {
            len += 1;
            match to_raw_offset(offset) {
                None => {
                    //           111w_oRRR (o is whether an offset follows, or it's 0, it's zero here)
                    bytes[0] = 0b1110_0000 | (reg as u8) | ((w as u8) << 4);
                }
                Some(raw_offset) => {
                    //           111w_oRRR (o is set because an offset follows)
                    bytes[0] = 0b1110_1000 | (reg as u8) | ((w as u8) << 4);
                    len += 1;
                    bytes[1] = raw_offset;
                }
            }
        }
    }

    len
}
pub fn write_both(mut bytes: &mut [u8], f_reg: FstArg, f: SndArg) -> usize {
    let mut len = 0;
    let reg = (f_reg as u8) << 5;

    #[cfg(debug_assertions)]
    {
        if let Ok(f_wide) = f.is_wide() {
            assert_eq!(f_reg.is_wide(), f_wide);
        }
    }

    match f {
        FullArg::Reg(reg2) => {
            len += 1;
            bytes[0] = reg | 0b0001_0000 | reg2 as u8;
        }
        FullArg::Byte(b) => {
            len += 2;
            bytes[0] = reg;
            bytes[1] = b;
        }
        FullArg::Wide(w) => {
            len += 1;
            bytes[0] = reg;
            bytes = &mut bytes[len..];
            write_wide(&mut len, w, bytes);
        }
        FullArg::RegRef(w, reg2, offset) => {
            debug_assert_eq!(w, f_reg.is_wide());
            len += 1;
            match to_raw_offset(offset) {
                None => {
                    bytes[0] = reg | 0b0001_1000 | reg2 as u8;
                }
                Some(raw_offset) => {
                    bytes[0] = reg | 0b0000_1100 | ((reg2 as u8) >> 1);
                    len += 1;
                    bytes[1] = (((reg2 as u8) & 1) << 7) | raw_offset;
                }
            }
        }
        FullArg::ImmRef(w, imm) => {
            debug_assert_eq!(w, f_reg.is_wide());
            len += 1;
            //           110w_xxxx
            bytes[0] = reg | 0b0000_1000;
            bytes = &mut bytes[len..];
            write_wide(&mut len, imm, bytes);
        }
    }

    len
}

impl OpAndArg {
    #[inline]
    pub fn write(&self, bytes: &mut [u8]) -> usize {
        match self.opcode.arg_identity() {
            a!(00) => 0,
            a!(01) => {
                bytes[0] = (unsafe { self.args.fst } as u8) << 5;
                1
            },
            a!(10) => {
                write_snd(bytes, unsafe { self.args.snd })
            },
            a!(11) => {
                let (f_reg, f) = unsafe { self.args.both };
                write_both(bytes, f_reg, f)
            },
        }
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

    fn test_read_write(op_and_arg: OpAndArg) {
        let mut bytes = [op_and_arg.opcode as u8, 0, 0, 0];

        let len = op_and_arg.write(&mut bytes[1..]);
        eprint!("{:02X?}", &bytes[..len+1]);
        eprintln!(" ++ {:02X?}", &bytes[len+1..]);
        eprintln!("{:08b} {:08b} {:08b} {:08b}", bytes[0], bytes[1], bytes[2], bytes[3]);
        let (oc_a, l) = read_instruction(bytes.read_iter_from(0u16));

        assert_eq!((len, op_and_arg), (l-1, oc_a));
    }

    #[test]
    fn ret() {
        test_read_write(OpAndArg::new_no(Opcode::RET).unwrap());
    }
    #[test]
    fn load_none_wide() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Wide(0xdead)).unwrap());
    }
    #[test]
    fn load_zero() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Wide(0x0000)).unwrap());
    }
    #[test]
    fn load_ba_4() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::Ba, FullArg::Wide(0x0004))).unwrap());
    }
    #[test]
    fn pop_ba() {
        test_read_write(OpAndArg::new_fst(Opcode::POP, Reg::Ba).unwrap());
    }
    #[test]
    fn load_ref_ba() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::RegRef(false, Reg::Ba, 0)).unwrap());
    }
    #[test]
    fn load_none_byte() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Byte(0xad)).unwrap());
    }
    #[test]
    fn load_none_ac_ba() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::Ac)).unwrap());
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::Ba)).unwrap());
    }
    #[test]
    fn load_none_ab_bb() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::Ab)).unwrap());
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::Bb)).unwrap());
    }
    #[test]
    fn int2() {
        test_read_write(OpAndArg::new_no(Opcode::INT2).unwrap());
    }
    #[test]
    fn store_reg_ref_ac() {
        test_read_write(OpAndArg::new_both(Opcode::STRAT, (Reg::Ac, FullArg::RegRef(true, Reg::Sp, -2))).unwrap());
    }
    #[test]
    fn load_reg_ref_ac_neg_big() {
        test_read_write(OpAndArg::new_both(Opcode::STRAT, (Reg::Ac, FullArg::RegRef(true, Reg::Ba, -64))).unwrap());
    }
    #[test]
    fn load_reg_ref_ac_pos_big() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::Ac, FullArg::RegRef(true, Reg::Ba, 64))).unwrap());
    }
    #[test]
    fn load_ref_ba_neg_big() {
        test_read_write(OpAndArg::new_snd(Opcode::STRA, FullArg::RegRef(true, Reg::Ba, -60)).unwrap());
    }
    #[test]
    fn load_ref_ba_pos_big() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::RegRef(true, Reg::Ba, 60)).unwrap());
    }
    #[test]
    fn load_into_ba_sp0() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::Ba, FullArg::RegRef(true, Reg::Sp, 0))).unwrap());
    }
    #[test]
    fn load_into_ba_sp1() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::Ba, FullArg::RegRef(true, Reg::Sp, 1))).unwrap());
    }
    #[test]
    fn addat_sp_4() {
        test_read_write(OpAndArg::new_both(Opcode::ADDAT, (Reg::Sp, FullArg::Wide(4))).unwrap());
    }
    #[test]
    #[should_panic]
    fn addat_sp_byte_4() {
        test_read_write(OpAndArg::new_both(Opcode::ADDAT, (Reg::Sp, FullArg::Byte(4))).unwrap());
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Reg {
    /// Accumulator
    Ac = 0b000,
    /// Accumulator as byte
    Ab = 0b001,
    /// Base pointer
    Bp = 0b010,
    /// Stack pointer
    Sp = 0b011,
    /// B accumulator
    Ba = 0b100,
    /// B accumulator as byte
    Bb = 0b101,
    /// Source
    Sr = 0b110,
    /// Destination
    Ds = 0b111,
}

impl Reg {
    /// The default register (the accumulator) with the given size
    /// Used for the instructions that only optionally take a first register argument
    const fn new(is_wide: bool) -> Self {
        if is_wide {
            Reg::Ac
        } else {
            Reg::Ab
        }
    }
    #[inline(always)]
    fn from_u8(b: u8) -> Self {
        debug_assert!(b < 0b1000);
        unsafe { std::mem::transmute(b) }
    }
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "ac" => Some(Self::Ac),
            "ab" => Some(Self::Ab),
            "bp" => Some(Self::Bp),
            "sp" => Some(Self::Sp),
            "ba" => Some(Self::Ba),
            "bb" => Some(Self::Bb),
            "sr" => Some(Self::Sr),
            "ds" => Some(Self::Ds),
            _ => None,
        }
    }
    /// Whether the value stored in this register is wide (otherwise a byte)
    pub const fn is_wide(&self) -> bool {
        use self::Reg::*;
        match *self {
            Ac => true,
            Ab => false,
            Bp => true,
            Sp => true,
            Ba => true,
            Bb => false,
            Sr => true,
            Ds => true,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FullArg {
    Reg(Reg),
    /// true if wide, false if byte
    ImmRef(bool, u16),
    /// If there's no offset, the value is 0
    /// 
    /// true if wide, false if byte
    RegRef(bool, Reg, i8),
    Byte(u8),
    Wide(u16),
}

impl FullArg {
    pub fn is_wide(&self) -> Result<bool, bool> {
        use self::FullArg::*;
        Ok(match self {
            Reg(r) => return Err(r.is_wide()),
            &ImmRef(w, _) | &RegRef(w, _, _) => w,
            Byte(_) => false,
            Wide(_) => true,
        })
    }
}