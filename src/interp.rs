use super::*;
use std::fmt;
extern crate std;

#[derive(Eq,Clone,PartialEq)]
pub struct Closure {
    arg: String,
    body: Expr,
    env: env::Env,
}

impl fmt::Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<closure>")
    }
}

fn eval<'a, 'b>(exp: Expr, env: env::Env) -> Value {
    match exp {
        Expr::Atom(v) => Value::Atom(v),
        Expr::Var(n) => env.find(n).unwrap_or(Value::Bottom),
        Expr::Lambda(arg, body) => Value::Closure(Closure {
            arg: arg, 
            body: *body,
            env: env,
        }),
        Expr::IsZero(body) => match eval(*body, env.clone()) {
            Value::Atom(Atom::Int(v)) => Value::Atom(Atom::Boolean(v == 0)),
            _ => Value::Bottom,
        },
        Expr::If(cond, c, a) => match eval(*cond, env.clone()) {
            Value::Atom(Atom::Boolean(true)) => eval(*c, env.clone()),
            Value::Atom(Atom::Boolean(false)) => eval(*a, env.clone()),
            _ => Value::Bottom,
        },
        Expr::BinOp(BinOp::Plus, a, b) => match (eval(*a, env.clone()), eval(*b, env.clone())) {
            (Value::Atom(Atom::Int(av)), Value::Atom(Atom::Int(bv))) => Value::Atom(Atom::Int(av+bv)),
            _ => Value::Bottom,
        },
        Expr::BinOp(BinOp::Mult, a, b) => match (eval(*a, env.clone()), eval(*b, env.clone())) {
            (Value::Atom(Atom::Int(av)), Value::Atom(Atom::Int(bv))) => Value::Atom(Atom::Int(av*bv)),
            _ => Value::Bottom,
        },
        Expr::BinOp(BinOp::Sub, a, b) => match (eval(*a, env.clone()), eval(*b, env.clone())) {
            (Value::Atom(Atom::Int(av)), Value::Atom(Atom::Int(bv))) => Value::Atom(Atom::Int(av-bv)),
            _ => Value::Bottom,
        },
        Expr::App(rator, rand) => match (eval(*rator, env.clone()), eval(*rand, env.clone())) {
            (Value::Closure(Closure { arg: a, body: b, env: e }), v) => eval(b, e.extend_env(a, v)),
            _ => Value::Bottom,
        },
        Expr::Let(var, binding, body) => {
            let bind_val = eval(*binding, env.clone());
            eval(*body, env.extend_env(var, bind_val))  
        },
        //_ => Value::Bottom,
    }
}

pub fn interp(exp: Expr) -> Value {
    let e = env::empty_env();
    eval(exp, e)
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
    use super::super::Value;

    #[derive(Debug, Eq, PartialEq)]
    struct Node {
        car: (String, Value),
        cdr: Env,
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    pub struct Env {
        env: Option<Rc<Node>>,
    }

    impl Env {
        pub fn extend_env(&self, n: String, v: Value) -> Env {
            Env {
                env: Some(Rc::new(Node {
                    car: (n, v),
                    cdr: self.clone(),
                }))
            }
        }

        pub fn find(&self, n: String) -> Option<Value> {
            self.env.as_ref().and_then(|node| {
                if node.car.0 == n {
                    Some(node.car.1.clone())
                } else {
                    node.cdr.find(n)
                }
            })
        }
    }

    pub fn empty_env() -> Env {
        Env { env: None }
    }

    #[test]
    fn it_works() {
        use super::super::Atom;

        let e = empty_env();
        let e = e.extend_env("a".to_string(), Value::Atom(Atom::Int(5)));
        let e = e.extend_env("b".to_string(), Value::Atom(Atom::Int(6)));
        let e = e.extend_env("a".to_string(), Value::Atom(Atom::Int(1)));

        let a = e.find("a".to_string());
        assert_eq!(a, Some(Value::Atom(Atom::Int(1))));
        assert_eq!(e.find("b".to_string()), Some(Value::Atom(Atom::Int(6))));
        assert_eq!(e.find("c".to_string()), None);
    }
}
