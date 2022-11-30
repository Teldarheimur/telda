use std::io::{Write, Read};

use crate::{cpu::{Registers, TrapMode, Register}, mem::Memory};

#[inline]
pub fn read_big_r(r: &mut Registers, m: &dyn Memory) -> u16 {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 8 {
        operand as u16 - 7
    } else {
        r.read(Register::new(operand))
    }
}
#[inline]
pub fn read_registers(r: &mut Registers, m: &dyn Memory) -> (Register, Register) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Register::new(operand >> 4), Register::new(operand & 0xf))
}
#[inline]
pub fn b_read_registers(r: &mut Registers, m: &dyn Memory) -> (Register, Register, Register) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Register::new((operand & 0b1111_0000) >> 4), Register::new((operand & 0b0000_1100) >> 2), Register::new((operand & 0b0000_0011) >> 0))
}
#[inline]
pub fn read_rh(r: &mut Registers, m: &dyn Memory) -> (Register, u16) {
    let low = m.read(r.pc);
    let high = m.read(r.pc+1);
    r.pc += 2;
    let r = Register::new(low >> 4);
    (r, u16::from_le_bytes([low & 0x0f, high]))
}
#[inline]
pub fn read_hr(r: &mut Registers, m: &dyn Memory) -> (u16, Register) {
    let low = m.read(r.pc);
    let high = m.read(r.pc+1);
    r.pc += 2;
    let r = Register::new(high & 0x0f);
    (((low as u16) << 4) | (high as u16 >> 4), r)
}
#[inline]
pub fn read_wide(r: &mut Registers, m: &dyn Memory) -> u16 {
    let val = m.read_wide(r.pc);
    r.pc += 2;
    val
}

pub static OP_HANDLERS: [OpHandler; 256] = [
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, write, halt, read, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    nop, push, call, jmp, ret, pop, ldstk, store, ststk, load, jez, jlt, jle, jgt, jge, jnz,
    jo, jno, jb, jae, ja, jbe, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, add, sub, and, or, xor, mul, bmul, div, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
];

pub type OpHandler = fn(&mut Registers, &mut dyn Memory);

fn invalid(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Invalid);
}
fn halt(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Halt);
}
fn write(r: &mut Registers, m: &mut dyn Memory) {
    let big_r = read_big_r(r, m) as u8;
    std::io::stdout().write_all(&[big_r]).unwrap()
}
fn read(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, _) = read_registers(r, m);
    let mut buf = [0];
    std::io::stdin().read(&mut buf).unwrap();
    r.write(r1, buf[0] as u16);
}

#[inline]
fn binop(r: &mut Registers, m: &mut dyn Memory, binop: fn(u16, u16) -> (u16, bool), ibinop: fn(i16, i16) -> (i16, bool)) {
    let (r1, r2) = read_registers(r, m);
    let big_r = read_big_r(r, m);
    let operand1 = r.read(r2);

    let (res, carry) = binop(operand1, big_r);
    let (ires, overflowing) = ibinop(operand1 as i16, big_r as i16);
    r.carry = carry;
    r.overflow = overflowing;
    r.sign = ires.is_negative();
    r.zero = res == 0;

    r.write(r1, res);
}

fn add(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, u16::overflowing_add, i16::overflowing_add);
}
fn sub(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, u16::overflowing_sub, i16::overflowing_sub);
}
fn and(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, |x, y| (x & y, false), |x, y| (x & y, false));
}
fn or(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, |x, y| (x | y, false), |x, y| (x | y, false));
}
fn xor(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, |x, y| (x ^ y, false), |x, y| (x ^ y, false));
}
fn mul(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = read_registers(r, m);
    let (r3, r4) = read_registers(r, m);

    let res = r.read(r3) as u32 * r.read(r4) as u32;
    let upper = (res >> 16) as u16;
    let lower = (res & 0xffff) as u16;

    r.carry = upper != 0;
    r.overflow = r.carry;
    r.zero = lower == 0;
    r.sign = (lower as i8).is_negative();

    r.write(r2, upper);
    r.write(r1, lower);
}
fn bmul(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2, r3) = b_read_registers(r, m);

    let (res, overflowing) = r.read(r2).overflowing_mul(r.read(r3));

    // TODO: check this
    r.carry = overflowing;
    r.overflow = overflowing;
    r.zero = res == 0;
    r.sign = (res as i8).is_negative();

    r.write(r1, res);
}
fn div(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = read_registers(r, m);
    let (r3, r4) = read_registers(r, m);

    let n1 = r.read(r3);
    let n2 = r.read(r4);
    if n2 == 0 {
        r.trap(TrapMode::ZeroDiv);
        return;
    }
    let upper = n1 / n2;
    let lower = n1 % n2;

    r.write(r2, upper);
    r.write(r1, lower);
}

fn nop(_: &mut Registers, _: &mut dyn Memory) {}
fn push(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_big_r(r, m);
    r.sp -= 2;
    m.write_wide(r.sp, w);
}
fn call(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_wide(r, m);
    let ret_addr = r.pc;
    r.pc = w;
    r.sp -= 2;
    m.write_wide(r.sp, ret_addr);
}
fn jmp(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_wide(r, m);
    r.pc = w;
}
fn ret(r: &mut Registers, m: &mut dyn Memory) {
    let b = m.read(r.pc);
    r.pc += 1; // NOTE: not necessary
    r.sp += b as u16;
    let ret_addr = m.read_wide(r.sp);
    r.sp += 2;
    r.pc = ret_addr;
}
fn pop(r: &mut Registers, m: &mut dyn Memory) {
    let (reg, _) = read_registers(r, m);

    let n = m.read_wide(r.sp);
    r.sp += 2;

    r.write(reg, n);
}
fn ldstk(r: &mut Registers, m: &mut dyn Memory) {
    let (reg, h) = read_rh(r, m);

    if reg.is_wide() {
        r.write(reg, m.read_wide(r.sp + h));
    } else if reg.is_byte() {
        r.write(reg, m.read(r.sp + h) as u16);
    }
}
fn store(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = read_registers(r, m);
    let big_r = read_big_r(r, m);

    let addr = r.read(r1) + r.read(r2);
    m.write_wide(addr, big_r);
}
fn ststk(r: &mut Registers, m: &mut dyn Memory) {
    let (h, reg) = read_hr(r, m);

    let val = r.read(reg);

    if reg.is_wide() {
        m.write_wide(r.sp + h, val);
    } else {
        m.write(r.sp + h, val as u8);
    }
}
fn load(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = read_registers(r, m);
    let (r3, _) = read_registers(r, m);

    let addr = r.read(r2) + r.read(r3);
    if r1.is_wide() {
        r.write(r1, m.read_wide(addr));
    } else {
        r.write(r1, m.read(addr) as u16);
    }
}

fn jez(r: &mut Registers, m: &mut dyn Memory) {
    if r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jlt(r: &mut Registers, m: &mut dyn Memory) {
    if r.sign != r.overflow {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jle(r: &mut Registers, m: &mut dyn Memory) {
    if r.sign != r.overflow && r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jgt(r: &mut Registers, m: &mut dyn Memory) {
    if r.sign == r.overflow && !r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jge(r: &mut Registers, m: &mut dyn Memory) {
    if r.sign == r.overflow {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jnz(r: &mut Registers, m: &mut dyn Memory) {
    if !r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jo(r: &mut Registers, m: &mut dyn Memory) {
    if r.overflow {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jno(r: &mut Registers, m: &mut dyn Memory) {
    if !r.overflow {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn ja(r: &mut Registers, m: &mut dyn Memory) {
    if !r.carry && !r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jae(r: &mut Registers, m: &mut dyn Memory) {
    if !r.carry {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jb(r: &mut Registers, m: &mut dyn Memory) {
    if r.carry {
        jmp(r, m);
    } else { r.pc += 2; }
}
fn jbe(r: &mut Registers, m: &mut dyn Memory) {
    if r.carry || r.zero {
        jmp(r, m);
    } else { r.pc += 2; }
}
