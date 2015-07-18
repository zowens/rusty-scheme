#![feature(plugin, subslice_offset)]
#![plugin(plex)]
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Var(String),
    IsZero(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Letrec(String, Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BinOp {
    Plus,
    Mult,
    Sub,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Atom {
    Int(i32),
    Boolean(bool),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Atom(Atom),
    Closure,
    Bottom,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Atom(Atom::Int(i)) => write!(f, "{}", i),
            &Value::Atom(Atom::Boolean(true)) => write!(f, "#t"),
            &Value::Atom(Atom::Boolean(false)) => write!(f, "#f"),
            &Value::Closure => write!(f, "#<closure>"),
            &Value::Bottom => write!(f, "#<TYPE ERROR>"),
        }
    }
}

pub mod parser;
pub mod interp;
pub mod lex;
