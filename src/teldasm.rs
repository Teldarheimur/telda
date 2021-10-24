use std::io::Read;
use std::fmt::{self, Display};

use super::is::Reg;

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::is::Reg::*;
        match self {
            Ac => write!(f, "$ac"),
            Ab => write!(f, "$ab"),
            Bp => write!(f, "$bp"),
            Sp => write!(f, "$sp"),
            Ba => write!(f, "$ba"),
            Bb => write!(f, "$bb"),
            Sr => write!(f, "$sr"),
            Ds => write!(f, "$ds"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Data {
    Byte(u8),
    Wide(Wide),
    String(String),
    Char(char),
}

impl Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Data::Byte(b) => write!(f, "{}", b),
            Data::Wide(w) => write!(f, "{}", w),
            Data::String(s) => write!(f, "{:?}", s),
            Data::Char(c) => write!(f, "{:?}", c),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Wide {
    Literal(u16),
    Label(String),
}

impl Display for Wide {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Wide::Literal(w) => write!(f, "~{}", w),
            Wide::Label(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Arg {
    Reg(Reg),
    Byte(u8),
    Wide(Wide),
    RefToByte(Wide),
    RefToWide(Wide),
    RegRefToByte(Reg),
    RegRefToWide(Reg),
}

impl Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Reg(r) => write!(f, "{}", r),
            Arg::Byte(b) => write!(f, "{}", b),
            Arg::Wide(w) => write!(f, "{}", w),
            Arg::RefToByte(w) => write!(f, "[{}]", w),
            Arg::RefToWide(w) => write!(f, "~[{}]]", w),
            Arg::RegRefToByte(r) => write!(f, "[{}]", r),
            Arg::RegRefToWide(r) => write!(f, "~[{}]", r),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Line {
    Include(Vec<String>),
    Label(String),
    Data(Vec<Data>),
    Instruction(String, Option<Reg>, Option<Arg>)
}

impl Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Line::Include(paths) => {
                write!(f, "include ")?;
                let mut first = true;
                for path in paths {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", path)?;
                }
                Ok(())
            }
            Line::Label(s) => write!(f, "{}:", s),
            Line::Data(data) => {
                write!(f, "dat ")?;
                let mut first = true;
                for data in data {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", data)?;
                }
                Ok(())
            }
            Line::Instruction(s, None, None) => write!(f, "    {}", s),
            Line::Instruction(s, Some(r), None) => write!(f, "    {} {}", s, r),
            Line::Instruction(s, None, Some(arg)) => write!(f, "    {} {}", s, arg),
            Line::Instruction(s, Some(r), Some(arg)) => write!(f, "    {} {}, {}", s, r, arg),
        }
    }
}

pub type Source = Vec<Line>;

pub fn print_source(src: &Source) {
    for line in src {
        println!("{}", line);
    }
}

use std::num::ParseIntError;
use nom::{
    branch::alt,
    bytes::streaming::{is_not, take_while_m_n},
    character::streaming::{char, multispace1},
    combinator::{map, map_opt, map_res, value, verify},
    error::{FromExternalError, ParseError},
    multi::fold_many0,
    sequence::{delimited, preceded},
    IResult,
};

fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded(
        char('u'),
        delimited(char('{'), parse_hex, char('}')),
    );

    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
  let not_quote_slash = is_not("\"\\");

  verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
where E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

fn parse_string<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError> {
    let build_string = fold_many0(
        parse_fragment,
        String::new(),
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        }
    );

    delimited(char('"'), build_string, char('"'))(input)
}

macro_rules! s {
    ($e:expr) => {$e.to_owned()};
}

pub fn password_src() -> Source {
    use self::Data as Dat;
    use self::Line::*;
    vec![
        Include(vec![s!("lib/std.telda"), s!("lib/strcmp.telda")]),
        Label(s!("correct")),
        Data(vec![Dat::String(s!("Access Granted!")), Dat::Byte(0)]),
        Label(s!("wrong")),
        Data(vec![Dat::String(s!("Access Denied!")), Dat::Byte(0)]),
        Label(s!("prompt")),
        Data(vec![Dat::String(s!("Enter password:")), Dat::Byte(0)]),
        Label(s!("buf")),
        Data(vec![Dat::String(s!("000000000000000000000000000000000"))]),
        Label(s!("password")),
        Data(vec![Dat::String(s!("PASS")), Dat::Byte(0)]),

        Label(s!("start")),
        Instruction(s!("push"), None, Some(Arg::Wide(Wide::Label(s!("prompt"))))),
        Instruction(s!("call"), None, Some(Arg::Wide(Wide::Label(s!("Print"))))),

        Instruction(s!("push"), None, Some(Arg::Wide(Wide::Label(s!("buf"))))),
        Instruction(s!("call"), None, Some(Arg::Wide(Wide::Label(s!("Getl"))))),
        Instruction(s!("push"), None, Some(Arg::Wide(Wide::Label(s!("password"))))),
        Instruction(s!("call"), None, Some(Arg::Wide(Wide::Label(s!("Strcmp"))))),
        Instruction(s!("cmp"), Some(Reg::Ab), Some(Arg::Wide(Wide::Literal(0)))),

        Instruction(s!("jez"), None, Some(Arg::Wide(Wide::Label(s!("success"))))),

        Instruction(s!("push"), None, Some(Arg::Wide(Wide::Label(s!("wrong"))))),
        Instruction(s!("call"), None, Some(Arg::Wide(Wide::Label(s!("Print"))))),

        Instruction(s!("add"), Some(Reg::Sp), Some(Arg::Wide(Wide::Literal(8)))),

        Instruction(s!("jmp"), None, Some(Arg::Wide(Wide::Label(s!("start"))))),

        Label(s!("success")),
        Instruction(s!("push"), None, Some(Arg::Wide(Wide::Label(s!("correct"))))),
        Instruction(s!("call"), None, Some(Arg::Wide(Wide::Label(s!("Print"))))),
        Instruction(s!("halt"), None, None),
    ]
}

pub fn read_source<R: Read>(r: R) -> Result<Source, ()> {
    todo!()
}

/*
TODO parse whole .telda file into `Source` or `Line`s
TODO make ctelda use this
TODO incorporate new format into the binaries
TODO write new code for new instruction set
*/
