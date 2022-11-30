#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operand {
    Register(u8),
    /// 1-240
    Immediate(u8),
}

pub const NULL: u8 = 0x00;
pub const WRITE: u8 = 0x09;
pub const HALT: u8 = 0x0a;
pub const READ: u8 = 0x0b;

pub const NOP: u8 = 0x20;
pub const PUSH: u8 = 0x21;
pub const CALL: u8 = 0x22;
pub const JUMP: u8 = 0x23;
pub const RET: u8 = 0x24;
pub const POP: u8 = 0x25;
pub const LOAD: u8 = 0x26;
pub const STORE: u8 = 0x27;
pub const LDSTK: u8 = 0x28;
pub const STSTK: u8 = 0x29;
pub const JEZ: u8 = 0x2a;
pub const JLT: u8 = 0x2b;
pub const JLE: u8 = 0x2c;
pub const JGT: u8 = 0x2d;
pub const JGE: u8 = 0x2e;
pub const JNZ: u8 = 0x2f;
pub const JO: u8 = 0x30;
pub const JNO: u8 = 0x31;
pub const JC: u8 = 0x32;
pub const JNC: u8 = 0x33;
pub const JA: u8 = 0x34;
pub const JAE: u8 = 0x35;
pub const JB: u8 = 0x36;
pub const JBE: u8 = 0x37;

pub const ADD: u8 = 0x41;
pub const SUB: u8 = 0x42;
pub const AND: u8 = 0x43;
pub const OR: u8 = 0x44;
pub const XOR: u8 = 0x45;
pub const MUL: u8 = 0x46;
pub const BMUL: u8 = 0x47;
pub const DIV: u8 = 0x48;

mod handlers;
pub use handlers::*;
