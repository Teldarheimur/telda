pub const NULL: u8 = 0x00;
pub const HALT: u8 = 0x0a;
pub const CTF: u8 = 0x0b;
pub const RETH: u8 = 0x0d;

pub const NOP: u8 = 0x20;
pub const PUSH_B: u8 = 0x21;
pub const PUSH_W: u8 = 0x22;
pub const POP_B: u8 = 0x23;
pub const POP_W: u8 = 0x24;
pub const CALL: u8 = 0x25;
pub const RET: u8 = 0x26;
pub const STORE_BI: u8 = 0x27;
pub const STORE_WI: u8 = 0x28;
pub const STORE_BR: u8 = 0x29;
pub const STORE_WR: u8 = 0x2a;
pub const LOAD_BI: u8 = 0x2b;
pub const LOAD_WI: u8 = 0x2c;
pub const LOAD_BR: u8 = 0x2d;
pub const LOAD_WR: u8 = 0x2e;
pub const JEZ: u8 = 0x2f;
pub const JLT: u8 = 0x30;
pub const JLE: u8 = 0x31;
pub const JGT: u8 = 0x32;
pub const JGE: u8 = 0x33;
pub const JNZ: u8 = 0x34;
pub const JO: u8 = 0x35;
pub const JNO: u8 = 0x36;
pub const JA: u8 = 0x37;
pub const JAE: u8 = 0x38;
pub const JB: u8 = 0x39;
pub const JBE: u8 = 0x3a;

pub const LDI_B: u8 = 0x3f;
/// Also jump
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

mod handlers;
pub use handlers::*;
