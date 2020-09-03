pub use std::fmt::{Display, Debug, Formatter, Result};

use super::*;

// #[derive(Debug, Copy, Clone)]
struct DebugCode<T: FmtCode>(T);

impl<T: FmtCode> Debug for DebugCode<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

pub struct DisplayCode<T: FmtCode>(pub T);

impl<T: FmtCode> Display for DisplayCode<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

pub trait FmtCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result;
}

macro_rules! impl_trivial {
    ($($tdisp:ty),*; $($tdbg:ty),*) => {
        $(
        impl FmtCode for $tdisp {
            #[inline]
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                <$tdisp as Display>::fmt(self, f)
            }
        }
        )*
        $(
        impl FmtCode for $tdbg {
            #[inline]
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                <$tdbg as Debug>::fmt(self, f)
            }
        }
        )*
    };
}

impl_trivial! {
    u8,
    Integer;
    str,
    String,
    Box<str>
}

impl<'a, T: FmtCode> FmtCode for &'a T {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        (*self).fmt(f)
    }
}

impl<T: FmtCode> FmtCode for List<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_list()
            .entries(self.iter().map(|e| DebugCode(e)))
            .finish()
    }
}

struct EmptyWhenNone<T>(Option<T>);

impl<T: FmtCode> Debug for EmptyWhenNone<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.0 {
            Some(s) => s.fmt(f),
            None => write!(f, "")
        }
    }
}

impl FmtCode for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Byte => Display::fmt("char", f),
            Type::Int => Display::fmt("int", f),
            Type::Pointer(ty) => write!(f, "{:?}*", DebugCode(&**ty)),
            Type::Array(ty, length) => write!(f, "{:?}[{:?}]", DebugCode(&**ty), EmptyWhenNone(length.as_ref())),
            Type::Struct(ident) => write!(f, "struct {}", ident),
            Type::Enum(ident) => write!(f, "enum {}", ident),
            Type::Union(ident) => write!(f, "union {}", ident),
            Type::NamedType(ident) => write!(f, "{}", ident),
        }
    }
}

impl FmtCode for LiteralValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            LiteralValue::Byte(b) => Display::fmt(b, f),
            LiteralValue::Int(i) => Display::fmt(i, f),
            LiteralValue::Ref(val) => write!(f, "&{:?}", DebugCode(&**val)),
            LiteralValue::String(s) => Debug::fmt(s, f),
        }
    }
}

impl FmtCode for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Value(v) => FmtCode::fmt(v, f),
            Expr::Variable(ident) => write!(f, "{}", ident),
            Expr::FunctionApplication(id, exprs) => {
                let mut tuple = f.debug_tuple(id);
                for expr in exprs {
                    tuple.field(&DebugCode(expr));
                }
                tuple.finish()
            }
            Expr::Ref(expr) => write!(f, "&{:?}", DebugCode(&**expr)),
            Expr::Array(exprs) => FmtCode::fmt(exprs, f),
            Expr::Struct(ident, fields) => {
                let mut strct = f.debug_struct(&format!("struct {}", ident));
                for (name, expr) in fields {
                    strct.field(&name, &DebugCode(expr));
                }
                strct.finish()
            }
            Expr::Enum(ident) => Display::fmt(ident, f),

            Expr::Add(l, r) => write!(f, "({:?} + {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Sub(l, r) => write!(f, "({:?} - {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Mul(l, r) => write!(f, "({:?} * {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Div(l, r) => write!(f, "({:?} / {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Rem(l, r) => write!(f, "({:?} % {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::And(l, r) => write!(f, "({:?} & {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Or(l, r) => write!(f, "({:?} | {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Xor(l, r) => write!(f, "({:?} ^ {:?})", DebugCode(&**l), DebugCode(&**r)),
            Expr::Not(expr) => write!(f, "!{:?}", DebugCode(&**expr)),
        }
    }
}

impl FmtCode for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if ({:?}) {:?}", DebugCode(&self.condition), DebugCode(&self.block))?;
        if let Some(else_) = &self.else_ {
            FmtCode::fmt(&else_, f)
        } else {
            Ok(())
        }
    }
}
impl FmtCode for IfElse {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "else ")?;
        match self {
            IfElse::ElseIf(if_) => FmtCode::fmt(&**if_, f),
            IfElse::Else(b) => FmtCode::fmt(b, f),
        }
    }
}
impl FmtCode for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            writeln!(f, "    {:?};", DebugCode(statement))?;
        }
        write!(f, "}}")
    }
}
impl FmtCode for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::VariableDeclaration(vd) => FmtCode::fmt(vd, f),
            Statement::Assignment(ident, e) => write!(f, "{} = {:?}", ident, DebugCode(&e)),
            Statement::Expr(expr) => FmtCode::fmt(expr, f),
            Statement::If(if_) => FmtCode::fmt(if_, f),
            Statement::While(expr, b) => {
                write!(f, "while ({:?}) ", DebugCode(&expr))?;
                FmtCode::fmt(&b, f)
            }
            Statement::For(init, condition, inc, b) => {
                write!(f, "for ({:?}; {:?}; {:?}) ", init, condition, inc)?;
                FmtCode::fmt(&b, f)
            }
        }
    }
}
impl FmtCode for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.constant {
            write!(f, "const ")?;
        }
        FmtCode::fmt(&self.var_type, f)?;
        write!(f, " {}", &*self.name)?;
        if let Some(value) = &self.value {
            write!(f, " = {:?}", DebugCode(&value))?;
        }
        write!(f, ";")
    }
}

impl FmtCode for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Item::StaticVariable(vd) => {
                write!(f, "static ")?;
                FmtCode::fmt(vd, f)
            }
            Item::Function{return_type, name, arguments, body} => {
                FmtCode::fmt(return_type, f)?;
                write!(f, " {}(", name)?;
                for (ty, name) in arguments {
                    write!(f, "{:?} {}, ", DebugCode(&ty), name)?;
                }
                write!(f, ") ")?;
                FmtCode::fmt(body, f)
            }
        }
    }
}

impl FmtCode for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for item in &self.0 {
            writeln!(f, "{:?}", DebugCode(item))?;
        }
        Ok(())
    }
}