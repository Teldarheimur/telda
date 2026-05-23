use crate::blf4::{ByteRegister, WideRegister, *};
use super::ErrorType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceOperand {
    Byte(u8),
    Wide(u16),
    Number(i32),
    ByteReg(ByteRegister),
    WideReg(WideRegister),
    Label(Box<str>),
    CurrentLocation,
    Add(Box<(Self, Self)>),
    Sub(Box<(Self, Self)>),
    Mul(Box<(Self, Self)>),
    Div(Box<(Self, Self)>),
}

pub fn parse_args(args: &str) -> Result<Vec<SourceOperand>, ErrorType> {
    let mut s = args.trim_start();
    let mut args = Vec::new();
    if s.is_empty() {
        return Ok(args);
    }
    let (so, rest) = parse_arg(s)?;
    args.push(so);
    s = rest.trim_start();

    while let Some(rest) = s.strip_prefix(",") {
        let arg = rest.trim_start();
        let (so, rest) = parse_arg(arg)?;
        args.push(so);
        s = rest.trim_start();
    }

    if s.is_empty() {
        Ok(args)
    } else {
        Err(ErrorType::Other(format!("expected end of line, found {s:?}").into()))
    }
}
pub fn parse_one_arg(arg: &str) -> Result<SourceOperand, ErrorType> {
    let (so, rest) = parse_arg(arg)?;
    if rest.trim_start().is_empty() {
        Ok(so)
    } else {
        Err(ErrorType::IncorrectOperands(
            "operand too long? idk write smth that makes sense if this ever comes up",
        ))
    }
}

fn parse_arg(arg: &str) -> Result<(SourceOperand, &str), ErrorType> {
    parse_term(arg)
}
fn parse_term(arg: &str) -> Result<(SourceOperand, &str), ErrorType> {
    let (mut lhs, rest) = parse_factor(arg)?;
    let mut s = rest.trim_start();
    loop {
        if let Some(rest) = s.strip_prefix("+") {
            let (rhs, rest) = parse_factor(rest.trim_start())?;
            lhs = SourceOperand::Add(Box::new((lhs, rhs)));
            s = rest.trim_start();
        } else if let Some(rest) = s.strip_prefix("-") {
            let (rhs, rest) = parse_factor(rest.trim_start())?;
            lhs = SourceOperand::Sub(Box::new((lhs, rhs)));
            s = rest.trim_start();
        } else {
            break;
        }
    }
    Ok((lhs, s))
}
fn parse_factor(arg: &str) -> Result<(SourceOperand, &str), ErrorType> {
    let (mut lhs, rest) = parse_number(arg)?;
    let mut s = rest.trim_start();
    loop {
        if let Some(rest) = s.strip_prefix("*") {
            let (rhs, rest) = parse_number(rest.trim_start())?;
            lhs = SourceOperand::Mul(Box::new((lhs, rhs)));
            s = rest.trim_start();
        } else if let Some(rest) = s.strip_prefix("/") {
            let (rhs, rest) = parse_number(rest.trim_start())?;
            lhs = SourceOperand::Div(Box::new((lhs, rhs)));
            s = rest.trim_start();
        } else {
            break;
        }
    }
    Ok((lhs, s))
}
fn parse_number(arg: &str) -> Result<(SourceOperand, &str), ErrorType> {
    if let Some(rest) = arg.strip_prefix("%") {
        return Ok((SourceOperand::CurrentLocation, rest));
    } else if let Some(arg) = arg.strip_prefix('\'') {
        let (byte, rest) = parse_bytechar(arg.as_bytes())?;
        let Some(rest) = rest.strip_prefix(b"'") else {
            return Err(ErrorType::CharacterLiteralTooLong);
        };

        return Ok((SourceOperand::Byte(byte), str::from_utf8(rest).unwrap()));
    }

    let so;
    let mut radix = 10;
    let i = arg.find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-')).unwrap_or(arg.len());
    let (arg, rest) = arg.split_at(i);
    let mut num = arg;
    if let Some(new_num) = arg.strip_prefix("0x") {
        radix = 16;
        num = new_num;
    } else if let Some(new_num) = arg.strip_prefix("0b") {
        radix = 2;
        num = new_num;
    } else if let Some(new_num) = arg.strip_prefix("0o") {
        radix = 8;
        num = new_num;
    }

    if let Some(num) = num.strip_suffix('b') {
        so = u8::from_str_radix(num, radix)
            .ok()
            .or_else(|| i8::from_str_radix(num, radix).ok().map(|b| b as u8))
            .map(SourceOperand::Byte);
    } else if let Some(num) = num.strip_suffix('w') {
        so = u16::from_str_radix(num, radix)
            .ok()
            .or_else(|| i16::from_str_radix(num, radix).ok().map(|w| w as u16))
            .map(SourceOperand::Wide);
    } else {
        so = i32::from_str_radix(num, radix)
            .ok()
            .map(SourceOperand::Number);
    }

    Ok((
        if let Some(so) = so {
            so
        } else {
            match arg {
                "" => return Err(ErrorType::IncorrectOperands("empty label")),
                "r0b" => SourceOperand::ByteReg(R0B),
                "r1l" => SourceOperand::ByteReg(R1L),
                "r1h" => SourceOperand::ByteReg(R1H),
                "r2l" => SourceOperand::ByteReg(R2L),
                "r2h" => SourceOperand::ByteReg(R2H),
                "r3l" => SourceOperand::ByteReg(R3L),
                "r3h" => SourceOperand::ByteReg(R3H),
                "r4l" => SourceOperand::ByteReg(R4L),
                "r4h" => SourceOperand::ByteReg(R4H),
                "r5l" => SourceOperand::ByteReg(R5L),
                "r5h" => SourceOperand::ByteReg(R5H),
                "r6b" => SourceOperand::ByteReg(R6B),
                "r7b" => SourceOperand::ByteReg(R7B),
                "r8b" => SourceOperand::ByteReg(R8B),
                "r9b" => SourceOperand::ByteReg(R9B),
                "r10b" => SourceOperand::ByteReg(R10B),
                "r0" => SourceOperand::WideReg(R0),
                "r1" => SourceOperand::WideReg(R1),
                "r2" => SourceOperand::WideReg(R2),
                "r3" => SourceOperand::WideReg(R3),
                "r4" => SourceOperand::WideReg(R4),
                "r5" => SourceOperand::WideReg(R5),
                "r6" => SourceOperand::WideReg(R6),
                "r7" => SourceOperand::WideReg(R7),
                "r8" => SourceOperand::WideReg(R8),
                "r9" => SourceOperand::WideReg(R9),
                "r10" => SourceOperand::WideReg(R10),
                "rs" => SourceOperand::WideReg(RS),
                "rl" => SourceOperand::WideReg(RL),
                "rf" => SourceOperand::WideReg(RF),
                "rp" => SourceOperand::WideReg(RP),
                "rh" => SourceOperand::WideReg(RH),
                _ => SourceOperand::Label(arg.into()),
            }
        },
        rest,
    ))
}

// TODO: make private?
pub fn parse_bytechar(s: &[u8]) -> Result<(u8, &[u8]), ErrorType> {
    use self::ErrorType::*;

    let mut bs = s.iter();
    Ok(match bs.next().ok_or(UnexpectedEndOfString)? {
        b'\\' => match bs.next().ok_or(EscapeCharacterAtEnd)? {
            b'r' => (b'\r', &s[2..]),
            b't' => (b'\t', &s[2..]),
            b'n' => (b'\n', &s[2..]),
            b'0' => (b'\0', &s[2..]),
            b'\\' => (b'\\', &s[2..]),
            b'\'' => (b'\'', &s[2..]),
            b'\"' => (b'\"', &s[2..]),
            b'x' => (
                u8::from_str_radix(String::from_utf8_lossy(&s[2..4]).as_ref(), 16)
                    .map_err(|_| InvalidEscapeSequence)?,
                &s[4..],
            ),
            c => return Err(InvalidEscapeCharacter(*c)),
        },
        &c => (c, &s[1..]),
    })
}
