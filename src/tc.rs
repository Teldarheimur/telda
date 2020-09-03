use std::collections::BTreeMap;

#[macro_export]
macro_rules! s {
    ($s:expr) => {
        $s.to_owned().into_boxed_str()
    };
}

mod code;

pub use code::*;

pub type Integer = u16;
pub type Str = Box<str>;
pub type Identifier = Str;
pub type List<T> = Vec<T>;

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        return_type: Type,
        name: Str,
        arguments: List<(Type, Str)>,
        body: Block,
    },
    StaticVariable(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub constant: bool,
    pub var_type: Type,
    pub name: Str,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: List<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Assignment(Identifier, Expr),
    Expr(Expr),
    If(If),
    While(Expr, Block),
    For(VariableDeclaration, Expr, Box<Statement>, Block),
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub block: Block,
    pub else_: Option<IfElse>,
}
#[derive(Debug, Clone)]
pub enum IfElse {
    ElseIf(Box<If>),
    Else(Block)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(LiteralValue),
    Variable(Identifier),
    FunctionApplication(Identifier, List<Expr>),
    Ref(Box<Self>),
    Array(List<Self>),
    Struct(Identifier, BTreeMap<Identifier, Self>),
    Enum(Identifier),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Byte,
    Int,
    Pointer(Box<Self>),
    Array(Box<Self>, Option<Integer>),
    Struct(Identifier),
    Enum(Identifier),
    Union(Identifier),
    NamedType(Identifier),
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Byte(u8),
    Int(Integer),
    Ref(Box<Self>),
    String(Str),
    // Array(List<Self>),
    // Struct(BTreeMap<Identifier, Self>),
    // Enum(Identifier),
    // Union(Identifier, Box<Self>),
}

pub type Program = List<Item>;
