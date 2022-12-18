use crate::{cpu::{Registers, TrapMode, ByteRegister as Br, WideRegister as Wr, Cpu}, mem::Memory};

#[inline]
pub fn arg_byte_big_r(r: &mut Registers, m: &dyn Memory) -> u8 {
    let operand = m.read(r.pc);
    r.pc += 1;
    if operand >= 8 {
        operand - 7
    } else {
        r.read_byte(Br::new(operand))
    }
}
#[inline]
pub fn arg_wide_big_r(r: &mut Registers, m: &dyn Memory) -> u16 {
    let operand = m.read_wide(r.pc);
    r.pc += 2;
    if operand >= 8 {
        operand - 7
    } else {
        r.read_wide(Wr::new(operand as u8))
    }
}
#[inline]
pub fn arg_byte_registers(r: &mut Registers, m: &dyn Memory) -> (Br, Br) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Br::new((operand >> 4) & 0x7), Br::new(operand & 0x7))
}
#[inline]
pub fn arg_wide_registers(r: &mut Registers, m: &dyn Memory) -> (Wr, Wr) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Wr::new((operand >> 4) & 0x7), Wr::new(operand & 0x7))
}
#[inline]
pub fn arg_byte_wide_registers(r: &mut Registers, m: &dyn Memory) -> (Br, Wr) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Br::new((operand >> 4) & 0x7), Wr::new(operand & 0x7))
}
#[inline]
pub fn arg_wide_byte_registers(r: &mut Registers, m: &dyn Memory) -> (Wr, Br) {
    let operand = m.read(r.pc);
    r.pc += 1;
    (Wr::new((operand >> 4) & 0x7), Br::new(operand & 0x7))
}

#[inline]
pub fn arg_imm_byte(r: &mut Registers, m: &dyn Memory) -> u8 {
    let val = m.read(r.pc);
    r.pc += 1;
    val
}
#[inline]
pub fn arg_imm_wide(r: &mut Registers, m: &dyn Memory) -> u16 {
    let val = m.read_wide(r.pc);
    r.pc += 2;
    val
}

pub type OpHandler = fn(&mut Registers, &mut dyn Memory);

pub static OP_HANDLERS: [OpHandler; 256] = {
    let mut handlers: [OpHandler; 256] = [n; 256];

    use super::*;

    handlers[NULL as usize] = n;
    handlers[SETH as usize] = seth;
    handlers[HALT as usize] = halt;
    handlers[CTF as usize] = ctf;
    handlers[RETH as usize] = reth;
    handlers[NOP as usize] = nop;
    handlers[PUSH_B as usize] = push_b;
    handlers[PUSH_W as usize] = push_w;
    handlers[POP_B as usize] = pop_b;
    handlers[POP_W as usize] = pop_w;
    handlers[CALL as usize] = call;
    handlers[RET as usize] = ret;
    handlers[STORE_B as usize] = store_b;
    handlers[STORE_W as usize] = store_w;
    handlers[LOAD_B as usize] = load_b;
    handlers[LOAD_W as usize] = load_w;
    handlers[JUMP as usize] = jmp;
    handlers[JUMP_REG as usize] = jmp_reg;
    handlers[JEZ as usize] = jez;
    handlers[JLT as usize] = jlt;
    handlers[JLE as usize] = jle;
    handlers[JGT as usize] = jgt;
    handlers[JGE as usize] = jge;
    handlers[JNZ as usize] = jnz;
    handlers[JO as usize] = jo;
    handlers[JNO as usize] = jno;
    handlers[JB as usize] = jb;
    handlers[JAE as usize] = jae;
    handlers[JA as usize] = ja;
    handlers[JBE as usize] = jbe;
    handlers[ADD_B as usize] = add_b;
    handlers[ADD_W as usize] = add_w;
    handlers[SUB_B as usize] = sub_b;
    handlers[SUB_W as usize] = sub_w;
    handlers[AND_B as usize] = and_b;
    handlers[AND_W as usize] = and_w;
    handlers[OR_B as usize] = or_b;
    handlers[OR_W as usize] = or_w;
    handlers[XOR_B as usize] = xor_b;
    handlers[XOR_W as usize] = xor_w;
    handlers[MUL_B as usize] = mul_b;
    handlers[MUL_W as usize] = mul_w;
    handlers[MUL_WB as usize] = mul_wb;
    handlers[DIV_B as usize] = div_b;
    handlers[DIV_W as usize] = div_w;

    handlers
};

fn n(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Invalid);
}
fn halt(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Halt);
}


fn seth(r: &mut Registers, m: &mut dyn Memory) {
    r.trap_handler = arg_imm_byte(r, m);
}
fn ctf(r: &mut Registers, _: &mut dyn Memory) {
    r.trap = false;
}
fn reth(r: &mut Registers, m: &mut dyn Memory) {
    if !r.trap {
        r.trap(TrapMode::InvalidHandlerReturn);
        return
    }
    Cpu::pop_registers(r, m);
    r.trap = false;
}

#[inline]
fn binop_b(r: &mut Registers, m: &mut dyn Memory, binop: fn(u8, u8) -> (u8, bool), ibinop: fn(i8, i8) -> (i8, bool)) {
    let (r1, r2) = arg_byte_registers(r, m);
    let operand1 = r.read_byte(r2);
    let operand2 = arg_byte_big_r(r, m);

    let (res, carry) = binop(operand1, operand2);
    let (ires, overflowing) = ibinop(operand1 as i8, operand2 as i8);
    r.carry = carry;
    r.overflow = overflowing;
    r.sign = ires.is_negative();
    r.zero = res == 0;

    r.write_byte(r1, res);
}
#[inline]
fn binop_w(r: &mut Registers, m: &mut dyn Memory, binop: fn(u16, u16) -> (u16, bool), ibinop: fn(i16, i16) -> (i16, bool)) {
    let (r1, r2) = arg_wide_registers(r, m);
    let operand1 = r.read_wide(r2);
    let operand2 = arg_wide_big_r(r, m);

    let (res, carry) = binop(operand1, operand2);
    let (ires, overflowing) = ibinop(operand1 as i16, operand2 as i16);
    r.carry = carry;
    r.overflow = overflowing;
    r.sign = ires.is_negative();
    r.zero = res == 0;

    r.write_wide(r1, res);
}

fn add_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, u8::overflowing_add, i8::overflowing_add);
}
fn add_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, u16::overflowing_add, i16::overflowing_add);
}
fn sub_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, u8::overflowing_sub, i8::overflowing_sub);
}
fn sub_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, u16::overflowing_sub, i16::overflowing_sub);
}
fn and_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (x & y, false), |x, y| (x & y, false));
}
fn and_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (x & y, false), |x, y| (x & y, false));
}
fn or_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (x | y, false), |x, y| (x | y, false));
}
fn or_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (x | y, false), |x, y| (x | y, false));
}
fn xor_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (x ^ y, false), |x, y| (x ^ y, false));
}
fn xor_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (x ^ y, false), |x, y| (x ^ y, false));
}
fn mul_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_byte_registers(r, m);
    let (r3, r4) = arg_byte_registers(r, m);

    let res = r.read_byte(r3) as u16 * r.read_byte(r4) as u16;
    let [lower, upper] = res.to_le_bytes();

    r.carry = upper != 0;
    r.overflow = r.carry;
    r.zero = lower == 0;
    r.sign = (lower as i8).is_negative();

    r.write_byte(r1, upper);
    r.write_byte(r2, lower);
}
fn mul_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_wide_registers(r, m);
    let (r3, r4) = arg_wide_registers(r, m);

    let res = r.read_wide(r3) as u32 * r.read_wide(r4) as u32;
    let [lower1, lower2, upper1, upper2] = res.to_le_bytes();
    let lower = u16::from_le_bytes([lower1, lower2]);
    let upper = u16::from_le_bytes([upper1, upper2]);

    r.carry = upper != 0;
    r.overflow = r.carry;
    r.zero = lower == 0;
    r.sign = (lower as i16).is_negative();

    r.write_wide(r1, upper);
    r.write_wide(r2, lower);
}
fn mul_wb(r: &mut Registers, m: &mut dyn Memory) {
    let (wr1, _wr2) = arg_wide_registers(r, m);
    let (r2, r3) = arg_byte_registers(r, m);

    let res = r.read_byte(r2) as u16 * r.read_byte(r3) as u16;

    r.carry = res > 0xff;
    r.overflow = r.carry;
    r.zero = (res & 0xff) == 0;
    r.sign = (res as i16).is_negative();

    r.write_wide(wr1, res);
}
fn div_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_byte_registers(r, m);
    let (r3, r4) = arg_byte_registers(r, m);

    let n1 = r.read_byte(r3);
    let n2 = r.read_byte(r4);
    if n2 == 0 {
        r.trap(TrapMode::ZeroDiv);
        return;
    }
    let upper = n1 / n2;
    let lower = n1 % n2;

    r.write_byte(r1, upper);
    r.write_byte(r2, lower);
}
fn div_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_wide_registers(r, m);
    let (r3, r4) = arg_wide_registers(r, m);

    let n1 = r.read_wide(r3);
    let n2 = r.read_wide(r4);
    if n2 == 0 {
        r.trap(TrapMode::ZeroDiv);
        return;
    }
    let upper = n1 / n2;
    let lower = n1 % n2;

    r.write_wide(r1, upper);
    r.write_wide(r2, lower);
}

fn nop(_: &mut Registers, _: &mut dyn Memory) {}
fn push_b(r: &mut Registers, m: &mut dyn Memory) {
    let b = arg_byte_big_r(r, m);
    r.sp -= 1;
    m.write(r.sp, b);
}
fn push_w(r: &mut Registers, m: &mut dyn Memory) {
    let w = arg_wide_big_r(r, m);
    r.sp -= 2;
    m.write_wide(r.sp, w);
}
fn pop_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, _) = arg_byte_registers(r, m);

    let n = m.read(r.sp);
    r.sp += 1;

    r.write_byte(r1, n);
}
fn pop_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, _) = arg_wide_registers(r, m);

    let n = m.read_wide(r.sp);
    r.sp += 2;

    r.write_wide(r1, n);
}
fn call(r: &mut Registers, m: &mut dyn Memory) {
    let w = arg_imm_wide(r, m);
    let ret_addr = r.pc;
    r.pc = w;
    r.sp -= 2;
    m.write_wide(r.sp, ret_addr);
}
fn ret(r: &mut Registers, m: &mut dyn Memory) {
    let b = arg_imm_byte(r, m);
    let ret_addr = m.read_wide(r.sp);
    r.sp += 2 + b as u16;
    r.pc = ret_addr;
}
fn store_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_wide_byte_registers(r, m);
    let offset = arg_wide_big_r(r, m);

    let addr = r.read_wide(r1) + offset;
    m.write(addr, r.read_byte(r2));
}
fn store_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_wide_registers(r, m);
    let offset = arg_wide_big_r(r, m);

    let addr = r.read_wide(r1) + offset;
    m.write_wide(addr, r.read_wide(r2));
}
fn load_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_byte_wide_registers(r, m);
    let offset = arg_wide_big_r(r, m);

    let addr = r.read_wide(r2) + offset;
    r.write_byte(r1, m.read(addr));
}
fn load_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_wide_registers(r, m);
    let offset = arg_wide_big_r(r, m);

    let addr = r.read_wide(r2) + offset;
    r.write_wide(r1, m.read_wide(addr));
}
fn jmp(r: &mut Registers, m: &mut dyn Memory) {
    let w = arg_imm_wide(r, m);
    r.pc = w;
}
fn jmp_reg(r: &mut Registers, m: &mut dyn Memory) {
    let (rw, _) = arg_wide_registers(r, m);
    r.pc = r.read_wide(rw);
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
