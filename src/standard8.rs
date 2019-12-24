use crate::is::*;
use super::{Machine, Memory, Memory8Bit, Cpu, Signal};
use std::io::{Write, Read};

pub type StandardMachine = Machine<u8, Memory8Bit, StandardCpu>;

#[derive(Debug)]
pub struct StandardCpu {
    pc: u8,
    stack_pointer: u8,
    work: u8,
    b_counter: u8,
    flags: u8,
}

type InterpretedArgs = (Option<Reg>, Option<u8>);

impl StandardCpu {
    pub fn new(start: u8) -> Self {
        StandardCpu {
            pc: start,
            stack_pointer: 0xff,
            work: 0,
            b_counter: 0,
            flags: 0,
        }
    }
    fn reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::Sp => self.stack_pointer,
            Reg::Bc => self.b_counter,
            Reg::Acc | Reg::AccW => self.work,
        }
    }
    fn reg_mut(&mut self, reg: Reg) -> &mut u8 {
        match reg {
            Reg::Sp => &mut self.stack_pointer,
            Reg::Bc => &mut self.b_counter,
            Reg::Acc | Reg::AccW => &mut self.work,
        }
    }
    fn interpret_args<M: Memory<<Self as Cpu>::Index>>(&self, args: Args, memory: &M) -> InterpretedArgs {
        (
            args.fst,
            args.snd.map(|s| match s {
                FullArg::Reg(r) => self.reg(r),
                FullArg::OffsetReg(r, o) => (self.reg(r) as i8 + o as i8) as u8,
                FullArg::Ref(Reference::Val(w)) => memory.read(w as u8),
                FullArg::Ref(Reference::Reg{reg, offset}) => {
                    debug_assert!(offset < 0x100);
                    let addr = (self.reg(reg) as i8 + offset as i8) as u8;

                    memory.read(addr)
                }
                FullArg::Byte(b) => b,
                FullArg::Wide(w) => {debug_assert!(w < 0x100); w as u8},
            })
        )
    }
    fn arg_val_mut(&mut self, arg: InterpretedArgs, def: u8) -> (&mut u8, u8) {
        let val = (arg.1).unwrap_or(def);
        let reg = self.reg_mut((arg.0).unwrap_or(Reg::Acc));

        (reg, val)
    }
    fn arg_val_reg(&mut self, arg: InterpretedArgs, def: u8) -> (u8, u8) {
        let val = (arg.1).unwrap_or(def);
        let reg = self.reg((arg.0).unwrap_or(Reg::Acc));

        (reg, val)
    }
    fn sarg_u8(&self, arg: InterpretedArgs) -> u8 {
        match arg {
            (None, None) => self.work,
            (None, Some(n)) => n,
            (Some(_), _) => panic!("Invalid args"),
        }
    }
    fn farg_mut(&mut self, arg: InterpretedArgs) -> &mut u8 {
        match arg {
            (None, None) => &mut self.work,
            (Some(r), None) => self.reg_mut(r),
            _ => panic!("Invalid args"),
        }
    }
    #[inline]
    fn sub(&mut self, arg: InterpretedArgs) {
        let (reg, val) = self.arg_val_mut(arg, 1);

        let (work, o) = reg.overflowing_sub(val);
        *reg = work;
        self.flags &= 0b1111_0000;
        self.flags |= if o {
            0b1100
        } else if work == 0 {
            0b0001
        } else {
            0b0010
        };
    }
    #[inline]
    fn binop_overflowing(&mut self, arg: InterpretedArgs, op: fn(u8, u8) -> (u8, bool)) {
        let (reg, val) = self.arg_val_mut(arg, self.work);
        
        let (work, o) = op(*reg, val);
        *reg = work;
        self.flags &= 0b1111_0000;
        if o {
            self.flags |= 0b1000;
        }
    }
    #[inline]
    fn binop(&mut self, arg: InterpretedArgs, op: fn(u8, u8) -> u8) {
        let (reg, val) = self.arg_val_mut(arg, self.work);

        *reg = op(*reg, val);
        self.flags &= 0b1111_0000;
    }
    #[inline]
    fn cmp(&mut self, arg: InterpretedArgs) {
        let (reg, val) = self.arg_val_reg(arg, 0);

        use std::cmp::Ordering::*;

        self.flags &= 0b1111_0000;
        self.flags |= match reg.cmp(&val) {
            Greater => 0b0100,
            Less => 0b0010,
            Equal => 0b0001,
        };
    }
    #[inline]
    fn jmp(&mut self, arg: InterpretedArgs, relative: bool) {
        let location = self.sarg_u8(arg);

        self.pc = if relative {
            (self.pc as i8).wrapping_add(location as i8) as u8
        } else {
            location
        };
    }
}

use std::ops::{BitAnd, BitOr, BitXor};

impl Cpu for StandardCpu {
    type Index = u8;

    fn run<M: Memory<Self::Index>>(&mut self, memory: &mut M) -> Option<Signal> {
        let (opcode, args, len) = read_instruction8(memory.read_to_slice(self.pc, 3));
        let len = len as u8;
        self.pc += len;

        let cur_ins = opcode as u8;

        let arg = self.interpret_args(args.clone(), &*memory);

        match cur_ins {
            0b0100_0000..=0xff => unreachable!(),
            NOP => (),
            DEPLDL | DEPSTL => panic!("DEPRECATED"),
            RES6..=RESF => panic!("RESERVED {:02x} @ {:02X}", cur_ins, self.pc-len),
            INVALID => panic!("Invalid instruction call {:02x}!\t{:x?}", cur_ins, self),
            LOAD => {
                let (reg, val) = self.arg_val_mut(arg, 0);

                *reg = val;
            }
            STR => {
                let (reg, location) = self.arg_val_reg(arg, 0);
                
                memory.write(location, reg);
            }
            COMPARE => self.cmp(arg),
            SUB => self.sub(arg),
            ADD => if let None = arg.1 {
                *self.reg_mut((arg.0).unwrap_or(Reg::Acc)) += 1;
            } else {
                self.binop_overflowing(arg, u8::overflowing_add)
            },
            MUL => self.binop_overflowing(arg, u8::overflowing_mul),
            DIV => self.binop_overflowing(arg, u8::overflowing_div),
            REM => self.binop_overflowing(arg, u8::overflowing_rem),
            AND => self.binop(arg, u8::bitand),
            OR => self.binop(arg, u8::bitor),
            XOR => self.binop(arg, u8::bitxor),
            NOT => {
                let (reg, val) = self.arg_val_mut(arg, self.work);

                *reg = !val;
            }
            JUMP | JMPR => self.jmp(arg, cur_ins & 0b1000 == 0b1000),
            JIO | JIOR => if self.flags & 0b1000 == 0b1000 {
                self.jmp(arg, cur_ins & 0b1000 == 0b1000)
            }
            JEZ..=JNE | JEZR..= JNER => {
                let mask = cur_ins & 0b0111;

                if self.flags & mask != 0 {
                    self.jmp(arg, cur_ins & 0b1000 == 0b1000);
                }
            }
            PUSH | PUSHW => {
                self.stack_pointer -= 1;
                memory.write(self.stack_pointer, self.sarg_u8(arg));
            }
            POP | POPW => {
                *self.farg_mut(arg) = memory.read(self.stack_pointer);
                self.stack_pointer += 1;
            }
            RET => {
                self.pc = memory.read(self.stack_pointer);
                self.stack_pointer += 1;
            }
            CALL => {
                let call_location = self.sarg_u8(arg);
                self.stack_pointer -= 1;
                memory.write(self.stack_pointer, self.pc);

                self.pc = call_location;
            }


            INT1 => {
                let mut bytes = [0];
                std::io::stdin().read_exact(&mut bytes).unwrap();
                *self.farg_mut(arg) = bytes[0];
            }
            INT2 => {
                std::io::stdout().write_all(&[self.sarg_u8(arg)]).unwrap();
            }
            INT3 => {
                std::io::stderr().write_all(&[self.sarg_u8(arg)]).unwrap();
            }
            INT15 => {
                eprintln!("{:x?}", self);
                let mut stack = Vec::new();
                let mut i = self.stack_pointer;
                while i < 255 {
                    stack.push(memory.read(i));
                    i += 1;
                }
                eprintln!("Stack: {:x?}", stack);
            }
            i @ INT4 ..= INT14 => eprintln!("Interrupt{:X} called", i&0xf),
            HALT => return Some(Signal::PowerOff),
        }

        None
    }
}
