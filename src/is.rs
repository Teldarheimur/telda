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

instructions!{Opcode;
    INVALID; 0x00;
    LOAD, "ld", "load"; 0x01;
    LDL, "lda", "ldl"; 0x02;
    STR, "str", "store"; 0x03;
    STL, "stl"; 0x04;
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
    LSV, "ldsv", "lsv"; 0x26;
    SSV, "stsv", "ssv"; 0x27;

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