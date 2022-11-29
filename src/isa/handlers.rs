use std::io::{Write, Read};

use crate::{cpu::{Registers, TrapMode}, mem::Memory};

use super::Register;

fn read_big_r(r: &mut Registers, m: &dyn Memory) -> u16 {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 0x10 {
        operand as u16 - 0x9
    } else {
        r.read(operand)
    }
}

fn read_registers(r: &mut Registers, m: &dyn Memory) -> (Register, Register) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Register(operand >> 4), Register(operand & 0xf))
}
fn b_read_registers(r: &mut Registers, m: &dyn Memory) -> (Register, Register, Register) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Register((operand & 0b1111_0000) >> 4), Register((operand & 0b0000_1100) >> 2), Register((operand & 0b0000_0011) >> 0))
}

fn read_wide(r: &mut Registers, m: &dyn Memory) -> u16 {
    let low = m.read(r.pc);
    let high = m.read(r.pc+1);
    r.pc += 2;
    low as u16 | ((high as u16) << 8)
}

pub static OP_HANDLERS: [OpHandler; 256] = [
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, write, halt, read, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    nop, push, call, jmp, ret, pop, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
    add, sub, and, or, xor, mul, bmul, div, invalid, invalid, invalid, invalid, invalid, invalid, invalid, invalid,
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
    r.write(r1.0, buf[0] as u16);
}

fn binop(r: &mut Registers, m: &mut dyn Memory, binop: fn(u16, u16) -> (u16, bool), ibinop: fn(i16, i16) -> (i16, bool)) {
    let (r1, r2) = read_registers(r, m);
    let big_r = read_big_r(r, m);
    let operand1 = r.read(r2.0);

    let (res, carry) = binop(operand1, big_r);
    let (ires, overflowing) = ibinop(operand1 as i16, big_r as i16);
    r.carry = carry;
    r.overflow = overflowing;
    r.sign = ires.is_negative();
    r.zero = res == 0;

    r.write(r1.0, res);
}

fn add(r: &mut Registers, m: &mut dyn Memory) {
    binop(r, m, u16::overflowing_add, i16::overflowing_add);
}
fn sub(r: &mut Registers, m: &mut dyn Memory) {

    binop(r, m, u16::overflowing_add, i16::overflowing_sub);
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

    let res = r.read(r3.0) as u32 * r.read(r4.0) as u32;
    let upper = (res >> 16) as u16;
    let lower = (res & 0xffff) as u16;

    r.carry = upper != 0;
    r.overflow = r.carry;
    r.zero = lower == 0;
    r.sign = (lower as i8).is_negative();

    r.write(r2.0, upper);
    r.write(r1.0, lower);
}
fn bmul(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2, r3) = b_read_registers(r, m);

    let (res, overflowing) = r.read(r2.0).overflowing_mul(r.read(r3.0));

    // TODO: check this
    r.carry = overflowing;
    r.overflow = overflowing;
    r.zero = res == 0;
    r.sign = (res as i8).is_negative();

    r.write(r1.0, res);
}
fn div(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = read_registers(r, m);
    let (r3, r4) = read_registers(r, m);

    let n1 = r.read(r3.0);
    let n2 = r.read(r4.0);
    if n2 == 0 {
        r.trap(TrapMode::ZeroDiv);
        return;
    }
    let upper = n1 / n2;
    let lower = n1 % n2;

    r.write(r2.0, upper);
    r.write(r1.0, lower);
}

fn nop(_: &mut Registers, _: &mut dyn Memory) {}
fn push(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_big_r(r, m);
    r.sp -= 2;
    m.write(r.sp, w as u8);
    m.write(r.sp+1, (w >> 8) as u8);
}
fn call(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_wide(r, m);
    let ret_addr = r.pc;
    r.pc = w;
    r.sp -= 2;
    m.write(r.sp, ret_addr as u8);
    m.write(r.sp+1, (ret_addr >> 8) as u8);
}
fn jmp(r: &mut Registers, m: &mut dyn Memory) {
    let w = read_wide(r, m);
    r.pc = w;
}
fn ret(r: &mut Registers, m: &mut dyn Memory) {
    let b = m.read(r.pc);
    r.pc += 1; // NOTE: not necessary
    r.sp += b as u16;
    let mut ret_addr = m.read(r.sp) as u16;
    ret_addr |= (m.read(r.sp+1) as u16) << 8;
    r.sp += 2;
    r.pc = ret_addr;
}
fn pop(r: &mut Registers, m: &mut dyn Memory) {
    let (reg, _) = read_registers(r, m);

    let mut n = m.read(r.sp) as u16;
    n |= (m.read(r.sp+1) as u16) << 8;
    r.sp += 2;

    r.write(reg.0, n);
}
// TODO: write load, store, and conditional jump instructions