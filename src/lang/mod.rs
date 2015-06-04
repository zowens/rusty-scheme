pub fn print_stuff() {
    //let e = Expr::Mult(Box::new(Expr::Atom(Value::Int(10))), Box::new(Expr::Atom(Value::Int(5))));
    let e = Expr::App(Box::new(Expr::Lambda("x".to_string(), Box::new(Expr::Var("x".to_string())))), Box::new(Expr::Atom(Atom::Int(5))));
    println!("{:?}", interp::interp(e));
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Var(String),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Atom {
    Int(i32),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Atom(Atom),
    Closure(interp::Closure),
    Bottom,
}

pub mod interp {
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
            Expr::Var(n) => env.as_ref().and_then(|m| m.find(&n)).unwrap_or(Value::Bottom),
            Expr::Lambda(arg, body) => Value::Closure(Closure {
                arg: arg, 
                body: *body,
                env: env,
            }),
            Expr::Plus(a, b) => match (eval(*a, env.clone()), eval(*b, env.clone())) {
                (Value::Atom(Atom::Int(av)), Value::Atom(Atom::Int(bv))) => Value::Atom(Atom::Int(av+bv)),
                _ => Value::Bottom,
            },
            Expr::Mult(a, b) => match (eval(*a, env.clone()), eval(*b, env.clone())) {
                (Value::Atom(Atom::Int(av)), Value::Atom(Atom::Int(bv))) => Value::Atom(Atom::Int(av*bv)),
                _ => Value::Bottom,
            },
            Expr::App(rator, rand) => match (eval(*rator, env.clone()), eval(*rand, env.clone())) {
                (Value::Closure(Closure { arg: a, body: b, env: e }), v) => eval(b, env::extend_env(a, v, e)),
                _ => Value::Bottom,
            },
            //_ => Value::Bottom,
        }
    }

    pub fn interp<'a>(exp: Expr) -> Value {
        let e = env::empty_env();
        eval(exp, e)
    }

    mod env {
        use std::rc::Rc;


        #[derive(Debug, Eq, PartialEq)]
        pub struct Node {
            car: (String, super::super::Value),
            cdr: Env,
        }

        pub type Env = Option<Rc<Node>>;

        impl Node {
            pub fn find(&self, var: &str) -> Option<super::super::Value> {
                if &self.car.0 == var {
                    Some(self.car.1.clone())
                } else {
                    self.cdr.as_ref().and_then(|b| b.find(var))
                }
            }
        }

        pub fn extend_env(n: String, v: super::super::Value, d: Env) -> Env {
            Some(Rc::new(Node { 
                car: (n, v),
                cdr: d
            }))
        }

        pub fn empty_env() -> Env {
            None
        }

        #[test]
        fn it_works() {
            let e = empty_env();
            let e = extend_env(String::from_str("a"), super::super::Value::Int(5), e);
            let e = extend_env(String::from_str("b"), super::super::Value::Int(6), e);
            let e = extend_env(String::from_str("a"), super::super::Value::Int(1), e);
            
            let a = e.as_ref().unwrap().find("a");
            assert_eq!(a, Some(super::super::Value::Int(1)));
            assert_eq!(e.as_ref().unwrap().find("b"), Some(super::super::Value::Int(6)));
            assert_eq!(e.as_ref().unwrap().find("c"), None);
        }

    }
}
