pub const NULL: u8 = 0x00;
pub const HALT: u8 = 0x0a;
pub const CTF: u8 = 0x0b;
pub const SYSCALL: u8 = 0xc;
pub const RETH: u8 = 0x0d;

pub const USR: u8 = 0x10;
pub const VMON: u8 = 0x11;
pub const VMOFF: u8 = 0x12;
pub const PSTORE: u8 = 0x13;
pub const PLOAD: u8 = 0x14;

pub const NOP: u8 = 0x20;
pub const PUSH_B: u8 = 0x21;
pub const PUSH_W: u8 = 0x22;
pub const POP_B: u8 = 0x23;
pub const POP_W: u8 = 0x24;
pub const ABS_CALL: u8 = 0x25;
pub const RET: u8 = 0x26;
pub const STORE_BI: u8 = 0x27;
pub const STORE_WI: u8 = 0x28;
pub const STORE_BR: u8 = 0x29;
pub const STORE_WR: u8 = 0x2a;
pub const LOAD_BI: u8 = 0x2b;
pub const LOAD_WI: u8 = 0x2c;
pub const LOAD_BR: u8 = 0x2d;
pub const LOAD_WR: u8 = 0x2e;
pub const ABS_JEZ: u8 = 0x2f;
pub const ABS_JLT: u8 = 0x30;
pub const ABS_JLE: u8 = 0x31;
pub const ABS_JGT: u8 = 0x32;
pub const ABS_JGE: u8 = 0x33;
pub const ABS_JNZ: u8 = 0x34;
pub const ABS_JO: u8 = 0x35;
pub const ABS_JNO: u8 = 0x36;
pub const ABS_JA: u8 = 0x37;
pub const ABS_JAE: u8 = 0x38;
pub const ABS_JB: u8 = 0x39;
pub const ABS_JBE: u8 = 0x3a;

pub const LDI_B: u8 = 0x3f;
/// Also absolute jump
pub const LDI_W: u8 = 0x40;

pub const ADD_B: u8 = 0x41;
pub const ADD_W: u8 = 0x42;
pub const SUB_B: u8 = 0x43;
pub const SUB_W: u8 = 0x44;
pub const AND_B: u8 = 0x45;
pub const AND_W: u8 = 0x46;
pub const OR_B: u8 = 0x47;
pub const OR_W: u8 = 0x48;
pub const XOR_B: u8 = 0x49;
pub const XOR_W: u8 = 0x4a;
pub const SHL_B: u8 = 0x4b;
pub const SHL_W: u8 = 0x4c;
pub const ASR_B: u8 = 0x4d;
pub const ASR_W: u8 = 0x4e;
pub const LSR_B: u8 = 0x4f;
pub const LSR_W: u8 = 0x50;

pub const DIV_B: u8 = 0x51;
pub const DIV_W: u8 = 0x52;
pub const MUL_B: u8 = 0x53;
pub const MUL_W: u8 = 0x54;
pub const ADC_B: u8 = 0x55;
pub const ADC_W: u8 = 0x56;
pub const SBB_B: u8 = 0x57;
pub const SBB_W: u8 = 0x58;

pub const R_CALL: u8 = 0x60;
pub const R_JUMP: u8 = 0x61;
pub const R_JEZ: u8 = 0x62;
pub const R_JLT: u8 = 0x63;
pub const R_JLE: u8 = 0x64;
pub const R_JGT: u8 = 0x65;
pub const R_JGE: u8 = 0x66;
pub const R_JNZ: u8 = 0x67;
pub const R_JO: u8 = 0x68;
pub const R_JNO: u8 = 0x69;
pub const R_JA: u8 = 0x6a;
pub const R_JAE: u8 = 0x6b;
pub const R_JB: u8 = 0x6c;
pub const R_JBE: u8 = 0x6d;
pub const COND_RES: u8 = 0x6e;
pub const SET_IF: u8 = 0x6f;

mod handlers;
pub use handlers::*;
