use super::*;

impl<I: InPort, O: OutPort, M: Memory<<StandardCpu<I, O> as Cpu>::Index>> InstructionHandler for CpuAndMemory<'_, M, I, O> {
    type Fst = Reg;
    type Snd = SecArgs;
    type InterruptSignal = Signal;

    #[inline(always)]
    fn convert_fst(&self, fst: FstArg) -> Self::Fst {
        fst
    }
    fn convert_snd(&self, snd: SndArg) -> Self::Snd {
        let (cpu, memory) = self;

        match snd {
            FullArg::Reg(r) => match cpu.reg(r) {
                Ok(w) => SecArgs::Wide(w),
                Err(b) => SecArgs::Byte(b),
            },
            FullArg::RegRef(is_wide, reg, offset) => {
                let addr = ((cpu.reg(reg).unwrap_or_else(|b| b as u16) as i16).wrapping_add(offset as i16)) as u16;

                if is_wide {
                    SecArgs::Wide(memory.read_index(addr))
                } else {
                    SecArgs::Byte(memory.read(addr))
                }
            }
            FullArg::ImmRef(is_wide, addr) => {
                if is_wide {
                    SecArgs::Wide(memory.read_index(addr))
                } else {
                    SecArgs::Byte(memory.read(addr))
                }
            }
            FullArg::Byte(b) => SecArgs::Byte(b),
            FullArg::Wide(w) => SecArgs::Wide(w),
        }
    }

    fn load(&mut self, reg: Self::Fst, val: Self::Snd) {
        let res = match self.0.reg_mut(reg) {
            Err(b) => {
                *b = val.u8();
                *b as i8 as i16 as u16
            }
            Ok(w) => {
                *w = val.u16();
                *w
            }
        };
        self.0.set_status_flags(res, false, false);
    }
    fn str(&mut self, reg: Self::Fst, location: Self::Snd){
        let (cpu, memory) = self;

        let location = location.u16();

        match cpu.reg(reg) {
            Ok(wide) => memory.write_index(location, wide),
            Err(acc) => memory.write(location, acc),
        }
    }
    #[inline]
    fn cmp(&mut self, reg: Self::Fst, val: Self::Snd) {
        // HACK
        let stash_reg = self.0.reg(reg);
        self.sub(reg, val);
        match self.0.reg_mut(reg) {
            Ok(r) => *r = stash_reg.unwrap(),
            Err(r) => *r = stash_reg.unwrap_err(),
        }
    }

    #[inline(always)]
    fn and(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitand, u16::bitand)
    }
    #[inline(always)]
    fn or(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitor, u16::bitor)
    }
    #[inline(always)]
    fn xor(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_no_overflow(reg, val, u8::bitxor, u16::bitxor)
    }
    #[inline(always)]
    fn not(&mut self, reg: Self::Fst) {
        let res = match self.0.reg_mut(reg) {
            Ok(reg) => {
                *reg = !*reg;
                *reg
            }
            Err(reg) => {
                *reg = !*reg;
                *reg as i8 as i16 as u16
            }
        };
        self.0.set_status_flags(res, false, false);
    }
    #[inline(always)]
    fn neg(&mut self, reg: Self::Fst) {
        let carry = self.0
            .reg(reg)
            .map(|x| x == 0)
            .unwrap_or_else(|x| x == 0);

        let (result, overflow) = match self.0.reg_mut(reg) {
            Ok(reg) => {
                let (res, of) = (*reg as i16).overflowing_neg();
                *reg = res as u16;
                (*reg, of)
            }
            Err(reg) => {
                let (res, of) = (*reg as i8).overflowing_neg();
                *reg = res as u8;
                (res as i16 as u16, of)
            }
        };
        self.0.set_status_flags(result, overflow, carry);
    }
    
    #[inline(always)]
    fn inc(&mut self, reg: Self::Fst) {
        self.add(reg, SecArgs::from_reg_with(reg, 1))
    }
    #[inline(always)]
    fn dec(&mut self, reg: Self::Fst) {
        self.sub(reg, SecArgs::from_reg_with(reg, 1))
    }
    fn sub(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_sub, u16::overflowing_sub, i8::overflowing_sub, i16::overflowing_sub)
    }
    #[inline(always)]
    fn add(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_add, u16::overflowing_add, i8::overflowing_add, i16::overflowing_add)
    }
    #[inline(always)]
    fn mul(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_mul, u16::overflowing_mul, i8::overflowing_mul, i16::overflowing_mul)
    }
    #[inline(always)]
    fn div(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_div, u16::overflowing_div, i8::overflowing_div, i16::overflowing_div)
    }
    #[inline(always)]
    fn rem(&mut self, reg: Self::Fst, val: Self::Snd) {
        self.0.binop_overflowing(reg, val, u8::overflowing_rem, u16::overflowing_rem, i8::overflowing_rem, i16::overflowing_rem)
    }

    fn push(&mut self, val: Self::Snd) {
        match val {
            SecArgs::Byte(b) => {
                self.0.stack_pointer -= 1;
                self.1.write(self.0.stack_pointer, b);
            }
            SecArgs::Wide(w) => {
                self.0.stack_pointer -= 2;
                self.1.write_index(self.0.stack_pointer, w);
            }
        }
    }
    fn pop(&mut self, reg: Self::Fst) {
        let sp = self.0.stack_pointer;

        let width = match self.0.reg_mut(reg) {
            Ok(reg) => {
                *reg = self.1.read_index(sp);
                2
            }
            Err(reg) => {
                *reg = self.1.read(sp);
                1
            }
        };
        self.0.stack_pointer += width;
    }
    fn store_at_stack_offset(&mut self, reg: Self::Fst, val: Self::Snd) {
        let (cpu, memory) = self;

        let offset = match val {
            // sign extend bytes
            SecArgs::Byte(b) => b as i8 as i16,
            SecArgs::Wide(w) => w as i16,
        };

        let location = (cpu.stack_pointer as i16).wrapping_add(offset) as u16;

        match cpu.reg(reg) {
            Ok(wide) => memory.write_index(location, wide),
            Err(acc) => memory.write(location, acc),
        }
    }
    #[inline]
    fn setd(&mut self, direction: bool) {
        self.0.set_flag(flags::DIR, direction);
    }
    fn sload(&mut self, reg: Self::Fst) {
        let (cpu, memory) = self;

        let source_location = cpu.source_pointer;

        let res;

        let inc = match cpu.reg_mut(reg) {
            Err(b) => {
                *b = memory.read(source_location);
                res = *b as i8 as i16 as u16;
                1
            }
            Ok(w) => {
                *w = memory.read_index(source_location);
                res = *w;
                2
            }
        };
        if cpu.get_flag(flags::DIR) {
            cpu.source_pointer += inc;
        } else {
            cpu.source_pointer -= inc;
        }
        self.0.set_status_flags(res, false, false);
    }

    fn sstore(&mut self, val: Self::Snd) {
        let inc  = match val {
            SecArgs::Byte(b) => {
                self.1.write(self.0.destination_pointer, b);
                1
            }
            SecArgs::Wide(w) => {
                self.1.write_index(self.0.destination_pointer, w);
                2
            }
        };
        if self.0.get_flag(flags::DIR) {
            self.0.destination_pointer += inc;
        } else {
            self.0.destination_pointer -= inc;
        }
    }
    fn smv(&mut self) {
        let incrementing = self.0.get_flag(flags::DIR);
        let b = self.1.read(self.0.source_pointer);
        self.1.write(self.0.destination_pointer, b);
        if incrementing {
            self.0.source_pointer += 1;
            self.0.destination_pointer += 1;
        } else {
            self.0.source_pointer -= 1;
            self.0.destination_pointer -= 1;
        }
        self.0.set_status_flags(b as i8 as i16 as u16, false, false);
    }
    #[inline]
    fn call(&mut self, location: Self::Snd) {
        #[cfg(feature = "print_instruction")]
        {
            self.0.indent += 1;
        }

        self.push(SecArgs::Wide(self.0.pc));

        self.0.pc = location.u16();
    }
    #[inline(always)]
    fn ret(&mut self) {
        #[cfg(feature = "print_instruction")]
        {
            self.0.indent = self.0.indent.saturating_sub(1);
        }

        self.0.pc = self.1.read_index(self.0.stack_pointer);
        
        self.0.stack_pointer += 2;
    }
    #[inline(always)]
    fn ret_add_sp(&mut self, bytes_to_deallocate: Self::Snd) {
        self.ret();

        self.0.stack_pointer += match bytes_to_deallocate {
            SecArgs::Byte(b) => b as u16,
            SecArgs::Wide(w) => w,
        };
    }

    #[inline(always)]
    fn jump(&mut self, location: Self::Snd) {
        self.0.pc = location.u16();
    }

    fn jof(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jno(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jsb(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) {
            self.jump(location);
        }
    }
    fn jns(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::SB) {
            self.jump(location);
        }
    }
    fn jez(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::EZ) {
            self.jump(location);
        }
    }
    fn jne(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::EZ) {
            self.jump(location);
        }
    }
    fn jca(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::CA) {
            self.jump(location);
        }
    }
    fn jnc(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::CA) {
            self.jump(location);
        }
    }
    fn jcz(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::CA | flags::EZ) {
            self.jump(location);
        }
    }
    fn jncz(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::CA | flags::EZ) {
            self.jump(location);
        }
    }

    fn jlt(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) != self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jge(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::SB) == self.0.get_flag(flags::OF) {
            self.jump(location);
        }
    }
    fn jle(&mut self, location: Self::Snd) {
        if self.0.get_flag(flags::EZ) || (self.0.get_flag(flags::SB) != self.0.get_flag(flags::OF)) {
            self.jump(location);
        }
    }
    fn jgt(&mut self, location: Self::Snd) {
        if !self.0.get_flag(flags::EZ) && (self.0.get_flag(flags::SB) == self.0.get_flag(flags::OF)) {
            self.jump(location);
        }
    }

    fn int0(&mut self) -> Option<Self::InterruptSignal> {
        Some(Signal::PowerOff)
    }
    fn int1(&mut self) -> Option<Self::InterruptSignal> {
        *self.0.accumulator.borrow_mut() = self.0.in_port.read_port();
        None
    }
    fn int2(&mut self) -> Option<Self::InterruptSignal> {
        self.0.out_port.write_port(*self.0.accumulator.borrow());
        None
    }
    #[inline(always)]
    fn int3(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int4(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int5(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int6(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int7(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int8(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int9(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int10(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int11(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int12(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int13(&mut self) -> Option<Self::InterruptSignal> { None }
    #[inline(always)]
    fn int14(&mut self) -> Option<Self::InterruptSignal> {
        let sr = self.1.read(self.0.source_pointer);
        let ds = self.1.read(self.0.destination_pointer);
        eprintln!("[$sr]: {:02x} {:?} {0:3}   [$ds]: {:02x} {:?} {2:3}",
            sr, sr as char, ds, ds as char);

        None
    }
    fn int15(&mut self) -> Option<Self::InterruptSignal> {
        eprintln!("{}", self.0.display());
        let mut stack = Vec::with_capacity((0xffff - self.0.stack_pointer) as usize);
        for i in self.0.stack_pointer..0xffff {
            stack.push(self.1.read(i));
        }
        eprintln!(" Stack: {:02x?}", stack);

        None
    }

    #[cfg(feature = "print_instruction")]
    fn print_instruction(&self, op_and_arg: OpAndArg) {
        for _ in 0..self.0.indent {
            eprint!("    ");
        }
        eprintln!("    {}", op_and_arg);
    }
}
