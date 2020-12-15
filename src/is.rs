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
    LOADAT, "load", "loadat"; 11 => 0x02;
    /// Write a value from register A to the given address in memory
    STRA, "str", "stora"; 10 => 0x03;
    /// Write a value from the given register to the given address in memory
    STRAT, "str", "storeat"; 11 => 0x04;
    /// Set the flags from comparing the A register with 0
    COMPARE0, "cmp", "compare0"; 00 => 0x05;
    /// Set the flags from comparing the A register with the given value
    COMPAREA, "cmp", "compara"; 10 => 0x06;
    /// Set the flags from comparing the given register with the given value
    COMPAREAT, "cmp", "compareat"; 11 => 0x07;

    ANDA, "and", "anda"; 10 => 0x08;
    ANDAT, "and", "andat"; 11 => 0x09;
    ORA, "or"; 10 => 0x0a;
    ORAT, "or"; 11 => 0x0b;
    XORA, "xor"; 10 => 0x0c;
    XORAT, "xor"; 11 => 0x0d;
    NOTA, "not"; 10 => 0x0e;
    NOTAT, "not"; 11 => 0x0f;

    INCA, "inc", "add", "inca"; 00 => 0x10;
    INCAT, "inc", "add", "incat"; 01 => 0x11;
    ADDA, "add", "adda"; 10 => 0x12;
    ADDAT, "add", "addat"; 11 => 0x13;
    DECA, "dec", "sub", "deca"; 00 => 0x14;
    DECAT, "dec", "sub", "decat"; 01 => 0x15;
    SUBA, "sub", "suba"; 10 => 0x16;
    SUBAT, "sub", "subat"; 11 => 0x17;
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

    /// Pushes given value as a byte to the stack.
    ///
    /// This decrements the stack register and then writes a byte to
    /// the memory location described by the stack register.
    PUSH, "psh", "push"; 10 => 0x20;
    /// Pushes given value as a wide to the stack.
    ///
    /// This decrements the stack register and then writes a wide to
    /// the memory location described by the stack register.
    PUSHW, "pshw", "pushw"; 10 => 0x21;
    /// Pops a byte off the stack into the given register.
    ///
    /// This reads a byte at the memory address described by the stack register
    /// and then increments the stack register
    POP, "pop"; 01 => 0x22;
    /// Pops a wide off the stack into the given register
    ///
    /// This reads a wide at the memory address described by the stack register
    /// and then increments the stack register
    POPW, "popw"; 01 => 0x23;
    
    /// Group 2 reserved instruction 4
    RES4; 00 => 0x24;
    /// Group 2 reserved instruction 5
    RES5; 00 => 0x25;
    /// Group 2 reserved instruction 6
    RES6; 00 => 0x26;
    /// Group 2 reserved instruction 7
    RES7; 00 => 0x27;
    /// Group 2 reserved instruction 8
    RES8; 00 => 0x28;
    /// Group 2 reserved instruction 9
    RES9; 00 => 0x29;
    /// Group 2 reserved instruction A
    RESA; 00 => 0x2a;
    /// Group 2 reserved instruction B
    RESB; 00 => 0x2b;
    /// Call
    CALL, "call"; 10 => 0x2c;
    /// Return
    RET, "ret"; 00 => 0x2d;
    /// Group 2 reserved instruction E
    RESE; 00 => 0x2e;
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
    fn pushw(&mut self, snd: Self::Snd);
    fn pop(&mut self, reg: Self::Fst);
    fn popw(&mut self, reg: Self::Fst);
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

    match op_and_arg.opcode {
        NOP => (),
        RES4 | RES5 | RES6 | RES7 | RES8 | RES9 | RESA | RESB | RESE | RESARITE | RESARITF => panic!("RESERVED"),
        INVALID => panic!("Invalid instruction call!"),
        LOADA => {
            let snd = op_and_arg.snd();

            h.load(h.convert_fst(Reg::Acc), h.convert_snd(snd));
        }
        LOADAT => {
            let (fst, snd) = op_and_arg.both();

            h.load(h.convert_fst(fst), h.convert_snd(snd));
        }
        STRA => {
            let snd = op_and_arg.snd();

            h.str(h.convert_fst(Reg::AccW), h.convert_snd(snd));
        }
        STRAT => {
            let (fst, snd) = op_and_arg.both();

            h.str(h.convert_fst(fst), h.convert_snd(snd));
        }
        COMPARE0 => {
            h.cmp(h.convert_fst(Reg::AccW), h.convert_snd(FullArg::Byte(0)));
        }
        COMPAREA => {
            h.cmp(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        COMPAREAT => {
            let (fst, snd) = op_and_arg.both();

            h.cmp(h.convert_fst(fst), h.convert_snd(snd));
        }
        DECA => {
            op_and_arg.none();
            h.dec(h.convert_fst(Reg::AccW));
        }
        DECAT => {
            let fst = op_and_arg.fst();

            h.dec(h.convert_fst(fst));
        }
        INCA => {
            op_and_arg.none();
            h.inc(h.convert_fst(Reg::AccW));
        }
        INCAT => {
            let fst = op_and_arg.fst();

            h.inc(h.convert_fst(fst));
        }
        SUBA => {
            h.sub(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        SUBAT => {
            let (fst, snd) = op_and_arg.both();

            h.sub(h.convert_fst(fst), h.convert_snd(snd));
        }
        ADDA => {
            h.add(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        ADDAT => {
            let (fst, snd) = op_and_arg.both();

            h.add(h.convert_fst(fst), h.convert_snd(snd));
        }
        MULA => {
            h.mul(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        MULAT => {
            let (fst, snd) = op_and_arg.both();

            h.mul(h.convert_fst(fst), h.convert_snd(snd));
        }
        DIVA => {
            h.div(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        DIVAT => {
            let (fst, snd) = op_and_arg.both();

            h.div(h.convert_fst(fst), h.convert_snd(snd));
        }
        REMA => {
            h.rem(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        REMAT => {
            let (fst, snd) = op_and_arg.both();

            h.rem(h.convert_fst(fst), h.convert_snd(snd));
        }

        ANDA => {
            h.and(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        ANDAT => {
            let (fst, snd) = op_and_arg.both();

            h.and(h.convert_fst(fst), h.convert_snd(snd));
        }
        ORA => {
            h.or(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        ORAT => {
            let (fst, snd) = op_and_arg.both();

            h.or(h.convert_fst(fst), h.convert_snd(snd));
        }
        XORA => {
            h.xor(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
        }
        XORAT => {
            let (fst, snd) = op_and_arg.both();

            h.xor(h.convert_fst(fst), h.convert_snd(snd));
        }
        NOTA => {
            h.not(h.convert_fst(Reg::AccW), h.convert_snd(op_and_arg.snd()));
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
        PUSHW => h.pushw(h.convert_snd(op_and_arg.snd())),
        POP => h.pop(h.convert_fst(op_and_arg.fst())),
        POPW => h.popw(h.convert_fst(op_and_arg.fst())),
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
            (Some(fst), Some(snd)) => OpAndArg {
                opcode: Opcode::from_str(s, a!(11)).valid()?,
                args: Args { both: (fst, snd) }
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
            let reg = Reg::from_u8(bytes.next().unwrap() & 0b11);
            
            Args { fst: reg }
        },
        a!(10) => Args { snd: {
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
        } },
        a!(11) => {
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
        FullArg::Ref(Reference::Reg { reg : r, offset: o}) => {
            if (o & 0b01111111_11111111) < 0x10 {
                let o = o as i8 as u8;
                
                len += 1;
                bytes[0] = ((r as u8) << 5) | ((o & 0b1000_0000) >> 3) | ((o & 0b1111) as u8);
            } else {
                len += 1;
                bytes[0] = 0b1001_0000 | ((r as u8) << 2);
                write_wide(&mut len, o as u16, &mut bytes[1..]);
            }
        }
        FullArg::OffsetReg(reg, o) => {
            len += 1;
            bytes[0] = 0b1111_0000 | ((reg as u8) << 2);
            write_wide(&mut len, o as u16, &mut bytes[1..]);
        }
        FullArg::Byte(b) => {
            len += 2;
            bytes[0] = 0b1000_0000;
            bytes[1] = b;  
        }
        FullArg::Wide(w) => {
            len += 1;
            bytes[0] = 0b1010_0000;
            write_wide(&mut len, w, &mut bytes[1..]);
        }
        FullArg::Reg(r) => {
            len += 1;
            bytes[0] = 0b1100_0000 | ((r as u8) << 2);
        }
        FullArg::Ref(Reference::Val(w)) => {
            len += 1;
            bytes[0] = 0b1110_0000;
            write_wide(&mut len, w, &mut bytes[1..]);
        }
    }

    len
}
pub fn write_both(mut bytes: &mut [u8], f_reg: FstArg, f: SndArg) -> usize {
    let mut len = 0;

    match f {
        FullArg::Ref(Reference::Reg { reg : r, offset: o}) => {
            debug_assert!(if false {
                o as i8 as i16 == o
            } else { true }, "impossible offset");
            if (o & 0b01111111_11111111) < 0x10 {
                let o = o as u8;

                len += 2;
                bytes[0] = ((r as u8) << 5) | ((o & 0b1000_0000) >> 3) | ((o & 0b1111) as u8);
                bytes[1] = f_reg as u8;
            } else {
                len += 1;
                bytes[0] = 0b1001_0000 | ((r as u8) << 2) | f_reg as u8;
                write_wide(&mut len, o as u16, &mut bytes[1..]);
            }
        }
        FullArg::OffsetReg(r, o) => {
            len += 1;
            bytes[0] = 0b1111_0000 | ((r as u8) << 2) | f_reg as u8;
            write_wide(&mut len, o as u16, &mut bytes[1..]);
        }
        FullArg::Byte(b) => {
            len += 2;
            bytes[0] = 0b1000_0000 | f_reg as u8;
            bytes[1] = b;  
        }
        FullArg::Wide(w) => {
            len += 1;
            bytes[0] = 0b1010_0000 | f_reg as u8;
            bytes = &mut bytes[1..];
            write_wide(&mut len, w, &mut bytes);
        }
        FullArg::Reg(r) => {
            len += 1;
            bytes[0] = 0b1101_0000 | ((r as u8) << 2) | f_reg as u8;
        }
        FullArg::Ref(Reference::Val(w)) => {
            len += 1;
            bytes[0] = 0b1110_0000 | f_reg as u8;
            bytes = &mut bytes[1..];
            write_wide(&mut len, w, &mut bytes);
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
                bytes[0] = unsafe { self.args.fst } as u8;
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
    fn load_none_byte() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Byte(0xad)).unwrap());
    }
    #[test]
    fn load_none_accw() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::AccW)).unwrap());
    }
    #[test]
    fn load_none_acc() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Reg(Reg::Acc)).unwrap());
    }
    #[test]
    fn int2() {
        test_read_write(OpAndArg::new_no(Opcode::INT2).unwrap());
    }
    #[test]
    fn store_reg_ref_accw() {
        test_read_write(OpAndArg::new_both(Opcode::STRAT, (Reg::AccW, FullArg::Ref(Reference::Reg{reg: Reg::Sp, offset: -2}))).unwrap());
    }
    #[test]
    fn load_reg_ref_accw_neg_big() {
        test_read_write(OpAndArg::new_both(Opcode::STRAT, (Reg::AccW, FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: -100}))).unwrap());
    }
    #[test]
    fn load_reg_ref_accw_pos_big() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::AccW, FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: 120}))).unwrap());
    }
    #[test]
    fn load_ref_accw_neg_big() {
        test_read_write(OpAndArg::new_snd(Opcode::STRA, FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: -100})).unwrap());
    }
    #[test]
    fn load_ref_accw_pos_big() {
        test_read_write(OpAndArg::new_snd(Opcode::LOADA, FullArg::Ref(Reference::Reg{reg: Reg::Bc, offset: 120})).unwrap());
    }
    #[test]
    fn load_into_bc_spp1() {
        test_read_write(OpAndArg::new_both(Opcode::LOADAT, (Reg::Bc, FullArg::Ref(Reference::Reg{reg: Reg::Sp, offset: 1}))).unwrap());
    }
    #[test]
    fn push_offset_reg() {
        test_read_write(OpAndArg::new_snd(Opcode::PUSH, FullArg::OffsetReg(Reg::Acc, 120)).unwrap());
    }
    #[test]
    fn sub_reg_offset_reg() {
        test_read_write(OpAndArg::new_both(Opcode::SUBAT, (Reg::Sp, FullArg::OffsetReg(Reg::Acc, 120))).unwrap());
    }
    #[test]
    fn push_offset_neg_reg() {
        test_read_write(OpAndArg::new_snd(Opcode::PUSH, FullArg::OffsetReg(Reg::Acc, -120)).unwrap());
    }
    #[test]
    fn add_reg_offset_neg_reg() {
        test_read_write(OpAndArg::new_both(Opcode::ADDAT, (Reg::Sp, FullArg::OffsetReg(Reg::Acc, -120))).unwrap());
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
