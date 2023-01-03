use crate::{cpu::{Registers, TrapMode, ByteRegister as Br, WideRegister as Wr, Cpu, R0}, mem::Memory, U4};

#[inline]
pub fn arg_pair<T, U, F1: FnOnce(U4) -> T, F2: FnOnce(U4) -> U>(r: &mut Registers, m: &mut dyn Memory, f1: F1, f2: F2) -> (T, U) {
    let operand = m.read(r.program_counter);
    r.program_counter += 1;
    let (a, b) = U4::paired(operand);
    (f1(a), f2(b))
}

#[inline]
pub fn arg_imm_byte(r: &mut Registers, m: &mut dyn Memory) -> u8 {
    let val = m.read(r.program_counter);
    r.program_counter += 1;
    val
}
#[inline]
pub fn arg_imm_wide(r: &mut Registers, m: &mut dyn Memory) -> u16 {
    let val = m.read_wide(r.program_counter);
    r.program_counter += 2;
    val
}

pub type OpHandler = fn(&mut Registers, &mut dyn Memory);

pub static OP_HANDLERS: [OpHandler; 256] = {
    let mut handlers: [OpHandler; 256] = [n; 256];

    use super::*;

    handlers[NULL as usize] = n;
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
    handlers[STORE_BI as usize] = store_bi;
    handlers[STORE_WI as usize] = store_wi;
    handlers[STORE_BR as usize] = store_br;
    handlers[STORE_WR as usize] = store_wr;
    handlers[LOAD_BI as usize] = load_bi;
    handlers[LOAD_WI as usize] = load_wi;
    handlers[LOAD_BR as usize] = load_br;
    handlers[LOAD_WR as usize] = load_wr;
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

    handlers[LDI_B as usize] = ldi_b;
    handlers[LDI_W as usize] = ldi_w;

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
    handlers[SHL_B as usize] = shl_b;
    handlers[SHL_W as usize] = shl_w;
    handlers[ASR_B as usize] = asr_b;
    handlers[ASR_W as usize] = asr_w;
    handlers[LSR_B as usize] = lsr_b;
    handlers[LSR_W as usize] = lsr_w;

    handlers[DIV_B as usize] = div_b;
    handlers[DIV_W as usize] = div_w;
    handlers[MUL_B as usize] = mul_b;
    handlers[MUL_W as usize] = mul_w;

    handlers
};

fn n(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Invalid);
}
fn halt(r: &mut Registers, _: &mut dyn Memory) {
    r.trap(TrapMode::Halt);
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
    let (r1, r2) = arg_pair(r, m, Br, Br);
    let (r3, r4) = arg_pair(r, m, Br, u8::from);

    let r2 = r.read_byte(r2);
    let r3 = r.read_byte(r3);
    if r4 != 0 {
        return r.trap(TrapMode::Invalid);
    }

    let (res, carry) = binop(r2, r3);
    let (ires, overflowing) = ibinop(r2 as i8, r3 as i8);
    r.carry = carry;
    r.overflow = overflowing;
    r.sign = ires.is_negative();
    r.zero = res == 0;

    r.write_byte(r1, res);
}
#[inline]
fn binop_w(r: &mut Registers, m: &mut dyn Memory, binop: fn(u16, u16) -> (u16, bool), ibinop: fn(i16, i16) -> (i16, bool)) {
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let (r3, r4) = arg_pair(r, m, Wr, u8::from);

    let r2 = r.read_wide(r2);
    let r3 = r.read_wide(r3);
    if r4 != 0 {
        return r.trap(TrapMode::Invalid);
    }

    let (res, carry) = binop(r2, r3);
    let (ires, overflowing) = ibinop(r2 as i16, r3 as i16);
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
fn shl_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (x << y, false), |x, y| (x << y, false));
}
fn shl_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (x << y, false), |x, y| (x << y, false));
}
fn asr_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (((x as i8) >> y) as u8, false), |x, y| (x >> y, false));
}
fn asr_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (((x as i16) >> y) as u16, false), |x, y| (x >> y, false));
}
fn lsr_b(r: &mut Registers, m: &mut dyn Memory) {
    binop_b(r, m, |x, y| (x >> y, false), |x, y| (((x as u8) >> y) as i8, false));
}
fn lsr_w(r: &mut Registers, m: &mut dyn Memory) {
    binop_w(r, m, |x, y| (x >> y, false), |x, y| (((x as u16) >> y) as i16, false));
}
fn mul_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Br, Br);
    let (r3, r4) = arg_pair(r, m, Br, Br);

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
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let (r3, r4) = arg_pair(r, m, Wr, Wr);

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
fn div_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Br, Br);
    let (r3, r4) = arg_pair(r, m, Br, Br);

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
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let (r3, r4) = arg_pair(r, m, Wr, Wr);

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
    let (b, z) = arg_pair(r, m, Br, u8::from);
    let b = r.read_byte(b);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    r.stack -= 1;
    m.write(r.stack, b);
}
fn push_w(r: &mut Registers, m: &mut dyn Memory) {
    let (w, z) = arg_pair(r, m, Wr, u8::from);
    let w = r.read_wide(w);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    r.stack -= 2;
    m.write_wide(r.stack, w);
}
fn pop_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, z) = arg_pair(r, m, Br, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }

    let n = m.read(r.stack);
    r.stack += 1;

    r.write_byte(r1, n);
}
fn pop_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, z) = arg_pair(r, m, Wr, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }

    let n = m.read_wide(r.stack);
    r.stack += 2;

    r.write_wide(r1, n);
}
fn call(r: &mut Registers, m: &mut dyn Memory) {
    let w = arg_imm_wide(r, m);
    r.link = r.program_counter;
    r.program_counter = w;
}
fn ret(r: &mut Registers, m: &mut dyn Memory) {
    let b = arg_imm_byte(r, m);
    r.stack += b as u16;
    r.program_counter = r.link;
}
fn store_bi(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Br);
    let offset = arg_imm_wide(r, m);

    let addr = r.read_wide(r1) + offset;
    m.write(addr, r.read_byte(r2));
}
fn store_br(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Br);
    let (r3, z) = arg_pair(r, m, Wr, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    let offset = r.read_wide(r3);

    let addr = r.read_wide(r1) + offset;
    m.write(addr, r.read_byte(r2));
}
fn store_wi(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let offset = arg_imm_wide(r, m);

    let addr = r.read_wide(r1) + offset;
    m.write_wide(addr, r.read_wide(r2));
}
fn store_wr(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let (r3, z) = arg_pair(r, m, Wr, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    let offset = r.read_wide(r3);

    let addr = r.read_wide(r1) + offset;
    m.write_wide(addr, r.read_wide(r2));
}
fn load_bi(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Br, Wr);
    let offset = arg_imm_wide(r, m);

    let addr = r.read_wide(r2) + offset;
    r.write_byte(r1, m.read(addr));
}
fn load_br(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Br, Wr);
    let (r3, z) = arg_pair(r, m, Wr, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    let offset = r.read_wide(r3);

    let addr = r.read_wide(r2) + offset;
    r.write_byte(r1, m.read(addr));
}
fn load_wi(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let offset = arg_imm_wide(r, m);

    let addr = r.read_wide(r2) + offset;
    r.write_wide(r1, m.read_wide(addr));
}
fn load_wr(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, r2) = arg_pair(r, m, Wr, Wr);
    let (r3, z) = arg_pair(r, m, Wr, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }
    let offset = r.read_wide(r3);

    let addr = r.read_wide(r2) + offset;
    r.write_wide(r1, m.read_wide(addr));
}

fn jez(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.zero, r, m);
}
fn jlt(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.sign != r.overflow, r, m);
}
fn jle(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.sign != r.overflow && r.zero, r, m);
}
fn jgt(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.sign == r.overflow && !r.zero, r, m);
}
fn jge(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.sign == r.overflow, r, m);
}
fn jnz(r: &mut Registers, m: &mut dyn Memory) {
    jif(!r.zero, r, m);
}
fn jo(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.overflow, r, m);
}
fn jno(r: &mut Registers, m: &mut dyn Memory) {
    jif(!r.overflow, r, m);
}
fn ja(r: &mut Registers, m: &mut dyn Memory) {
    jif(!r.carry && !r.zero, r, m);
}
fn jae(r: &mut Registers, m: &mut dyn Memory) {
    jif(!r.carry, r, m);
}
fn jb(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.carry, r, m);
}
fn jbe(r: &mut Registers, m: &mut dyn Memory) {
    jif(r.carry || r.zero, r, m);
}
fn jif(cond: bool, r: &mut Registers, m: &mut dyn Memory) {
    let location = arg_imm_wide(r, m);
    if cond {
        r.program_counter = location;
    }
}

fn ldi_b(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, z) = arg_pair(r, m, Br, u8::from);
    if z != 0 {
        return r.trap(TrapMode::Invalid);
    }

    let b = arg_imm_byte(r, m);

    r.write_byte(r1, b);
}
fn ldi_w(r: &mut Registers, m: &mut dyn Memory) {
    let (r1, o) = arg_pair(r, m, Wr, u8::from);

    let w = arg_imm_wide(r, m);

    match u8::from(o) {
        // ldi
        0 => r.write_wide(r1, w),
        // jmp, jump
        1 => {
            if r1 == R0 {
                // jmp imm
                r.program_counter = w;
            } else {
                // jmp r
                r.program_counter = r.read_wide(r1);
            }
        }
        _ => return r.trap(TrapMode::Invalid),
    }
}
