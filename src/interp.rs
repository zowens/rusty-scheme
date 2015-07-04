use super::*;
use std::fmt;
extern crate std;

#[derive(Eq,Clone,PartialEq)]
pub struct Closure<'a> {
    arg: &'a str,
    body: &'a Expr,
    env: env::Env<'a>,
}

impl<'a> fmt::Debug for Closure<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<closure>")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Value_<'a> {
    Atom(Atom),
    Closure(interp::Closure<'a>),
    Bottom,
}

fn eval<'a>(exp: &'a Expr, env: env::Env<'a>) -> Value_<'a> {
    match exp {
        &Expr::Atom(ref v) => Value_::Atom(v.clone()),
        &Expr::Var(ref n) => env.find(n).unwrap_or(Value_::Bottom),
        &Expr::Lambda(ref arg, ref body) => Value_::Closure(Closure {
            arg: arg, 
            body: body,
            env: env,
        }),
        &Expr::IsZero(ref body) => match eval(body, env.clone()) {
            Value_::Atom(Atom::Int(v)) => Value_::Atom(Atom::Boolean(v == 0)),
            _ => Value_::Bottom,
        },
        &Expr::If(ref cond, ref c, ref a) => match eval(cond, env.clone()) {
            Value_::Atom(Atom::Boolean(true)) => eval(c, env.clone()),
            Value_::Atom(Atom::Boolean(false)) => eval(a, env.clone()),
            _ => Value_::Bottom,
        },
        &Expr::BinOp(BinOp::Plus, ref a, ref b) => match (eval(a, env.clone()), eval(b, env.clone())) {
            (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Value_::Atom(Atom::Int(av+bv)),
            _ => Value_::Bottom,
        },
        &Expr::BinOp(BinOp::Mult, ref a, ref b) => match (eval(a, env.clone()), eval(b, env.clone())) {
            (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Value_::Atom(Atom::Int(av*bv)),
            _ => Value_::Bottom,
        },
        &Expr::BinOp(BinOp::Sub, ref a, ref b) => match (eval(a, env.clone()), eval(b, env.clone())) {
            (Value_::Atom(Atom::Int(av)), Value_::Atom(Atom::Int(bv))) => Value_::Atom(Atom::Int(av-bv)),
            _ => Value_::Bottom,
        },
        &Expr::App(ref rator, ref rand) => match (eval(rator, env.clone()), eval(rand, env.clone())) {
            (Value_::Closure(Closure { arg: a, body: b, env: e }), v) => eval(b, e.extend_env(a, v)),
            _ => Value_::Bottom,
        },
        &Expr::Let(ref var, ref binding, ref body) => {
            let bind_val = eval(binding, env.clone());
            eval(body, env.extend_env(var, bind_val))  
        },
        //_ => Value_::Bottom,
    }
}

pub fn interp<'a>(exp: &'a Expr) -> Value {
    let e = env::empty_env();
    match eval(exp, e) {
        Value_::Atom(v) => Value::Atom(v),
        Value_::Closure(_) => Value::Closure,
        Value_::Bottom => Value::Bottom,
    }
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
    ];

    let parse_str = |s| { 
        match super::parser::parse(super::lex::Lexer::new(s)) {
            Ok(e) => Ok(e),
            Err(e) => Err(format!("err parsing {}, err={:?}", s, e)),
        }
    };

    for p in exprs {
        assert_eq!(parse_str(p.0).map(|e| interp(e)), Ok(p.1));
    }
}

mod env {
    use std::rc::Rc;
    use super::Value_;

    #[derive(Debug, Eq, PartialEq)]
    struct Node<'a> {
        car: (&'a str, Value_<'a>),
        cdr: Env<'a>,
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    pub struct Env<'a> {
        env: Option<Rc<Node<'a>>>,
    }

    impl<'a> Env<'a> {
        pub fn extend_env(&self, n: &'a str, v: Value_<'a>) -> Env<'a> {
            Env {
                env: Some(Rc::new(Node {
                    car: (n, v),
                    cdr: self.clone(),
                }))
            }
        }

        pub fn find(&self, n: &str) -> Option<Value_<'a>> {
            self.env.as_ref().and_then(|node| {
                if node.car.0 == n {
                    Some(node.car.1.clone())
                } else {
                    node.cdr.find(n)
                }
            })
        }
    }

    pub fn empty_env<'a>() -> Env<'a> {
        Env { env: None }
    }

    #[test]
    fn it_works() {
        use super::super::Atom;

        let e = empty_env();
        let e = e.extend_env("a", Value::Atom(Atom::Int(5)));
        let e = e.extend_env("b", Value::Atom(Atom::Int(6)));
        let e = e.extend_env("a", Value::Atom(Atom::Int(1)));

        let a = e.find("a");
        assert_eq!(a, Some(Value::Atom(Atom::Int(1))));
        assert_eq!(e.find("b"), Some(Value::Atom(Atom::Int(6))));
        assert_eq!(e.find("c"), None);
    }
}
