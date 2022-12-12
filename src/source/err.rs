use std::error::Error as ErrorTrait;
use std::fmt::{self, Display};
pub(super) use std::io::Error as IoError;
pub(super) use std::result::Result as StdResult;

pub type Result<T> = StdResult<T, Error>;

pub type LineNumber = u32;

#[derive(Debug)]
pub enum ErrorType {
    InvalidOperand(Box<str>),
    UnknownInstruction(Box<str>),
    UnknownDirective(Box<str>),
    IoError(IoError),
    Other(Box<str>),
}

#[derive(Debug)]
pub struct Error {
    source: Box<str>,
    ln: LineNumber,
    error: ErrorType,
    next: Option<Box<Self>>,
}

impl Error {
    pub(super) fn new(s: impl Into<Box<str>>, ln: LineNumber, error: ErrorType) -> Self {
        Error { source: s.into(), ln, error, next: None, }
    }
    pub fn chain(mut self, second: Self) -> Self {
        self.chain_mut(second);
        self
    }
    fn chain_mut(&mut self, second: Self) {
        match &mut self.next {
            Some(first) => first.chain_mut(second),
            a @ None => *a = Some(Box::new(second)),
        }
    }
    // pub(super) fn set_line_number(self, ln: LineNumber) -> Self {
    //     Self { ln, .. self }
    // }
    // pub(super) fn set_source(self, source: Box<str>) -> Self {
    //     Self { source, .. self }
    // }
}

impl ErrorTrait for Error {
    fn source(&self) -> Option<&(dyn ErrorTrait + 'static)> {
        match &self.error {
            ErrorType::IoError(e) => Some(e),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut cur = Some(self);
        
        while let Some(e) = cur {
            let Self { source, ln, error, next } = e;

            write!(f, "{source}:{ln}: ")?;
            match error {
                ErrorType::InvalidOperand(s) => write!(f, "invalid operand: {s}"),
                ErrorType::UnknownInstruction(s) => write!(f, "unknown instruction: {s}"),
                ErrorType::UnknownDirective(s) => write!(f, "unknown directive: {s}"),
                ErrorType::IoError(e) => write!(f, "io error: {e}"),
                ErrorType::Other(s) => write!(f, "{s}"),
            }?;
            if next.is_some() {
                writeln!(f)?;
            }

            cur = next.as_ref().map(|e| &**e);
        }

        Ok(())
    }
}

impl From<IoError> for Error {
    fn from(e: IoError) -> Self {
        Self {
            ln: 0,
            source: "".to_owned().into_boxed_str(),
            error: ErrorType::IoError(e),
            next: None,
        }
    }
}
