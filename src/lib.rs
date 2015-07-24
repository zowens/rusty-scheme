#![feature(plugin, subslice_offset, box_syntax)]
#![plugin(plex)]
use std::fmt;
extern crate repl as repllib;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Cons(Box<Expr>, Box<Expr>),
    Car(Box<Expr>),
    Cdr(Box<Expr>),
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
    Nil, //< empty list
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Atom(Atom),
    Cons(Box<Value>, Box<Value>),
    Closure,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Atom(Atom::Int(i)) => write!(f, "{}", i),
            &Value::Atom(Atom::Boolean(true)) => write!(f, "#t"),
            &Value::Atom(Atom::Boolean(false)) => write!(f, "#f"),
            &Value::Atom(Atom::Nil) => write!(f, "()"),
            // TODO: recursive list repr
            &Value::Cons(ref a, ref b) => write!(f, "({} . {})", a, b),
            &Value::Closure => write!(f, "#<closure>"),
        }
    }
}

pub mod parser;
pub mod interp;
pub mod lex;
pub mod repl;

#[test]
fn test_value_display() {
    let tests = vec![
        (Value::Atom(Atom::Int(5)), "5"),
        (Value::Atom(Atom::Boolean(true)), "#t"),
        (Value::Atom(Atom::Boolean(false)), "#f"),
        (Value::Closure, "#<closure>"),
        (Value::Bottom, "#<TYPE ERROR>"),
        (Value::Cons(box Value::Atom(Atom::Int(5)), box Value::Atom(Atom::Int(6))), "(5 . 6)"),
        // TODO: fix this.. should be (5)
        (Value::Cons(box Value::Atom(Atom::Int(5)), box Value::Atom(Atom::Nil)), "(5 . ())"),
        (Value::Atom(Atom::Nil), "()"),
    ];

    for t in tests.iter() {
        assert_eq!(t.1, format!("{}", t.0));
    }
}
