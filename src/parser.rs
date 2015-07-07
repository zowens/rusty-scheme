use super::{Expr, BinOp, Atom};
use super::lex::Token::*;
use super::lex::*;

parser! {
    fn parse_(Token, Span);

    // combine two spans
    (a, b) {
        Span {
            lo: a.lo,
            hi: b.hi,
        }
    }

    exp: Expr {
        Ident(i) => Expr::Var(i),
        sexp[x] => x, 
        atom[x] => x,
    }

    /*exprs: Vec<Expr> {
        => vec![],
        exprs[mut st] expr[e] RParen => {
            st.push(e);
            st
        }
    }*/

    sexp: Expr {
        LParen Lambda LParen Ident(v) RParen exp[body] RParen => Expr::Lambda(v, Box::new(body)),
        LParen Let LParen LParen Ident(v) exp[bind] RParen RParen exp[body] RParen => Expr::Let(v, Box::new(bind), Box::new(body)),
        LParen Letrec LParen LParen Ident(v) exp[bind] RParen RParen exp[body] RParen => Expr::Letrec(v, Box::new(bind), Box::new(body)),
        LParen If exp[t] exp[c] exp[a] RParen => Expr::If(Box::new(t), Box::new(c), Box::new(a)),
        LParen Plus exp[a] exp[b] RParen => Expr::BinOp(BinOp::Plus, Box::new(a), Box::new(b)),
        LParen Minus exp[a] exp[b] RParen => Expr::BinOp(BinOp::Sub, Box::new(a), Box::new(b)),
        LParen Star exp[a] exp[b] RParen => Expr::BinOp(BinOp::Mult, Box::new(a), Box::new(b)),
        LParen IsZero exp[a] RParen => Expr::IsZero(Box::new(a)),
        LParen exp[rator] exp[rand] RParen => Expr::App(Box::new(rator), Box::new(rand)),
    }

    atom: Expr {
        Integer(i) => Expr::Atom(Atom::Int(i)),
        True => Expr::Atom(Atom::Boolean(true)),
        False => Expr::Atom(Atom::Boolean(false)),
    }
}

pub fn parse<I: Iterator<Item=(Token, Span)>>(i: I) -> Result<Expr, (Option<(Token, Span)>, &'static str)> {
    parse_(i)
}

#[test]
fn test_parser() {
    let tests = vec![
        ("5", Expr::Atom(Atom::Int(5))),
        ("12345", Expr::Atom(Atom::Int(12345i32))),
        ("(+ 5 7)", Expr::BinOp(BinOp::Plus, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(* 5 7)", Expr::BinOp(BinOp::Mult, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(- 5 7)", Expr::BinOp(BinOp::Sub, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(+ (* 5 7) 7)", Expr::BinOp(BinOp::Plus, Box::new(Expr::BinOp(BinOp::Mult, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(* (+ 5 7) 7)", Expr::BinOp(BinOp::Mult, Box::new(Expr::BinOp(BinOp::Plus, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("x", Expr::Var("x".to_string())),
        ("x12_34-xdghy$%^&@+", Expr::Var("x12_34-xdghy$%^&@+".to_string())),
         ("(lambda (x) x)", Expr::Lambda("x".to_string(), Box::new(Expr::Var("x".to_string())))),
        ("((lambda (x) x) 5)", 
         Expr::App(
             Box::new(Expr::Lambda("x".to_string(), Box::new(Expr::Var("x".to_string())))),
             Box::new(Expr::Atom(Atom::Int(5i32))))),

        ("((lambda (x) (+ x x)) 5)", 
         Expr::App(
             Box::new(Expr::Lambda("x".to_string(), Box::new(Expr::BinOp(BinOp::Plus, Box::new(Expr::Var("x".to_string())), Box::new(Expr::Var("x".to_string())))))),
             Box::new(Expr::Atom(Atom::Int(5i32))))),

        ("(zero? (+ 10 11))",
            Expr::IsZero(
              Box::new(Expr::BinOp(BinOp::Plus, Box::new(Expr::Atom(Atom::Int(10))), Box::new(Expr::Atom(Atom::Int(11))))))),

        ("#t", Expr::Atom(Atom::Boolean(true))),
        ("#f", Expr::Atom(Atom::Boolean(false))),
        ("(if x y z)",
          Expr::If(
              Box::new(Expr::Var("x".to_string())),
              Box::new(Expr::Var("y".to_string())),
              Box::new(Expr::Var("z".to_string())))),

        ("(let ((x 5)) x)",
          Expr::Let(
              "x".to_string(), 
              Box::new(Expr::Atom(Atom::Int(5))),
              Box::new(Expr::Var("x".to_string())))),
        ("(letrec ((x 5)) x)",
          Expr::Letrec(
              "x".to_string(), 
              Box::new(Expr::Atom(Atom::Int(5))),
              Box::new(Expr::Var("x".to_string())))),
    ];

    for tc in tests {
        // append fake span, check result
        let parsed = Lexer::new(tc.0);
        let r = parse(parsed).unwrap();
        assert_eq!(tc.1, r);
    }
}
