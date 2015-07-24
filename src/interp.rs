use super::*;
use std::rc::Rc;
extern crate std;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Closure<'a> {
    arg: &'a str, 
    body: &'a Expr, 
    env: Env<'a>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value_<'a> {
    Atom(Atom),
    Closure(Closure<'a>),
    Cons(Box<Value_<'a>>, Box<Value_<'a>>),
}

impl<'a> Value_<'a> {
    fn to_value(self) -> Value {
        match self {
            Value_::Atom(a) => Value::Atom(a),
            Value_::Closure(_) => Value::Closure,
            Value_::Cons(a, d) => Value::Cons(box a.to_value(), box d.to_value()),
        }
    }
}

fn eval<'a>(exp: &'a Expr, env: Env<'a>) -> Result<Value_<'a>, String> {
    match exp {
        &Expr::Atom(ref v) => Ok(Value_::Atom(v.clone())),
        &Expr::Var(ref n) => env.find(n).ok_or(format!("unexpected var {}", n)),
        &Expr::Lambda(ref arg, ref body) => Ok(Value_::Closure(Closure { arg: arg, body: body, env: env })),
        &Expr::IsZero(ref body) => {
            let v = try!(eval(body, env.clone()));
            match v {
                Value_::Atom(Atom::Int(v)) => Ok(Value_::Atom(Atom::Boolean(v == 0))),
                v  => Err(format!("zero?: invalid argument {}", v.to_value())),
            }
        },
        &Expr::If(ref cond, ref c, ref a) => {
            let v = try!(eval(cond, env.clone()));
            match v {
                Value_::Atom(Atom::Boolean(false)) => eval(a, env.clone()),
                _ => eval(c, env.clone()),
            }
        },
        &Expr::BinOp(BinOp::Plus, ref a, ref b) => {
            let av = try!(eval(a, env.clone()));
            let bv = try!(eval(b, env.clone()));
            match (av, bv) {
                (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Ok(Value_::Atom(Atom::Int(av+bv))),
                (av, bv) => Err(format!("attempt to add {} and {}", bv.to_value(), av.to_value())),
            }
        },
        &Expr::BinOp(BinOp::Mult, ref a, ref b) => {
            let av = try!(eval(a, env.clone()));
            let bv = try!(eval(b, env.clone()));
            match (av, bv) {
                (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Ok(Value_::Atom(Atom::Int(av*bv))),
                (av, bv) => Err(format!("attempt to multiply {} and {}", bv.to_value(), av.to_value())),
            }
        },
        &Expr::BinOp(BinOp::Sub, ref a, ref b) => {
            let av = try!(eval(a, env.clone()));
            let bv = try!(eval(b, env.clone()));
            match (av, bv) {
                (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Ok(Value_::Atom(Atom::Int(av-bv))),
                (av, bv) => Err(format!("attempt to subtract {} from {}", bv.to_value(), av.to_value())),
            }
        },
        &Expr::App(ref rator, ref rand) => {
            let ratorv = try!(eval(rator, env.clone()));
            let randv = try!(eval(rand, env.clone()));
            match (ratorv, randv) {
                (Value_::Closure(c), v) => eval(c.body, c.env.extend_env(c.arg, v)),
                (v, _) => Err(format!("attempt to apply non-procedure {}", v.to_value())),
            }
        },
        &Expr::Cons(ref a, ref d) => {
            let av = try!(eval(a, env.clone()));
            let dv = try!(eval(d, env.clone()));
            Ok(Value_::Cons(box av, box dv))
        },
        &Expr::Car(ref c) => {
            let p = try!(eval(c, env.clone()));
            match p {
                Value_::Cons(a, _) => Ok(*a),
                p => Err(format!("attempt to apply car to non-pair {}", p.to_value())),
            }
        },
        &Expr::Cdr(ref c) => {
            let p = try!(eval(c, env.clone()));
            match p {
                Value_::Cons(_, d) => Ok(*d),
                p => Err(format!("attempt to apply car to non-pair {}", p.to_value())),
            }
        },
        &Expr::Let(ref var, ref binding, ref body) => {
            let bind_val = try!(eval(binding, env.clone()));
            eval(body, env.extend_env(var, bind_val))  
        },
        &Expr::Letrec(ref var, ref binding, ref body) => {
            let bindv = try!(eval(binding, env.clone()));
            match bindv {
                Value_::Closure(c) => eval(body, env.extend_env_rec(var, c.clone())),
                v => eval(body, env.extend_env(var, v)),
            }
        },
        //_ => Value_::Bottom,
    }
}

pub fn interp<'a>(exp: &'a Expr) -> Result<Value, String> {
    let e = empty_env();
    eval(exp, e).map(|v| v.to_value())
}


#[test]
fn test_interp() {
    let exprs = vec![
        ("5", Value::Atom(Atom::Int(5))),
        ("(+ 5 5)", Value::Atom(Atom::Int(10))),
        ("(+ (+ 10 10) 5)", Value::Atom(Atom::Int(25))),
        ("(* (+ 10 10) 5)", Value::Atom(Atom::Int(100))),
        ("(zero? 10)", Value::Atom(Atom::Boolean(false))),
        ("(zero? (+ 5 5))", Value::Atom(Atom::Boolean(false))),
        ("(zero? 0)", Value::Atom(Atom::Boolean(true))),
        ("#t", Value::Atom(Atom::Boolean(true))),
        ("#f", Value::Atom(Atom::Boolean(false))),
        ("(if (zero? 0) 5 6)", Value::Atom(Atom::Int(5))),
        ("(if (zero? 1) 5 6)", Value::Atom(Atom::Int(6))),
        ("(let ((x 5)) (+ 1 x))", Value::Atom(Atom::Int(6))),
        ("((lambda (x) x) 5)", Value::Atom(Atom::Int(5))),
        ("((lambda (x) (+ x x)) 10)", Value::Atom(Atom::Int(20))),
        ("(((lambda (f) (lambda (g) (f (g 5)))) (lambda (x) (+ x 10))) (lambda (y) (- y 1)))", Value::Atom(Atom::Int(14))),
        ("(letrec ((fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) (fact 5))", Value::Atom(Atom::Int(120))),
        ("(letrec ((x 5)) x)", Value::Atom(Atom::Int(5))),
        ("(car (cons (+ 1 2) (+ 3 4)))", Value::Atom(Atom::Int(3))),
        ("(cdr (cons (+ 1 2) (+ 3 4)))", Value::Atom(Atom::Int(7))),
    ];

    let parse_str = |s| { 
        match super::parser::parse(super::lex::Lexer::new(s)) {
            Ok(e) => Ok(e),
            Err(e) => Err(format!("err parsing {}, err={:?}", s, e)),
        }
    };

    for p in exprs {
        assert_eq!(parse_str(p.0).as_ref().map(|e| interp(e)), Ok(p.1));
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Env_<'a> {
    Empty,
    ExtendEnv(&'a str, Value_<'a>, Rc<Env_<'a>>),
    ExtendEnvRec(&'a str, Closure<'a>, Rc<Env_<'a>>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Env<'a> {
    e_: Rc<Env_<'a>>,
}

impl<'a> Env<'a> {
    pub fn extend_env(&self, n: &'a str, v: Value_<'a>) -> Env<'a> {
        Env {
            e_: Rc::new(Env_::ExtendEnv(n, v, self.e_.clone())),
        }
    }

    pub fn extend_env_rec(&self, n: &'a str, c: Closure<'a>) -> Env<'a> {
        Env {
            e_: Rc::new(Env_::ExtendEnvRec(n, c, self.e_.clone())),
        }
    }

    fn find_inner(env: &Env_<'a>, n: &'a str) -> Option<Value_<'a>> {
        match env {
            &Env_::Empty => None,
            &Env_::ExtendEnv(nm, ref v, ref next_env) => if nm == n {
                Some(v.clone())
            } else {
                Env::find_inner(&next_env, n)
            },
            &Env_::ExtendEnvRec(nm, ref c, ref next_env) => if nm == n {
                Some(Value_::Closure(
                        Closure {
                            arg: c.arg,
                            body: c.body,
                            env: c.env.extend_env_rec(nm, c.clone()),
                        }))
            } else {
                Env::find_inner(&next_env, n)
            }
        }
    }

    pub fn find(&self, n: &'a str) -> Option<Value_<'a>> {
        Env::find_inner(&self.e_, n)
    }
}

fn empty_env<'a>() -> Env<'a> {
    Env { e_: Rc::new(Env_::Empty) }
}

#[test]
fn it_works() {
    use super::Atom;

    let e = empty_env();
    let e = e.extend_env("a", Value_::Atom(Atom::Int(5)));
    let e = e.extend_env("b", Value_::Atom(Atom::Int(6)));
    let e = e.extend_env("a", Value_::Atom(Atom::Int(1)));

    let a = e.find("a");
    assert_eq!(a, Some(Value_::Atom(Atom::Int(1))));
    assert_eq!(e.find("b"), Some(Value_::Atom(Atom::Int(6))));
    assert_eq!(e.find("c"), None);
}
