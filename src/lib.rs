#![feature(plugin, subslice_offset)]
#![plugin(plex)]

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Var(String),
    IsZero(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
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

pub mod parser;
pub mod interp;
pub mod lex;
