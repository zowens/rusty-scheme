use parser_combinators::*;
use parser_combinators::primitives::{State, Stream};
use super::{Expr, Atom, BinOp};

fn atom_int<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    many1(digit())
        .map(|string: String| Expr::Atom(Atom::Int(string.parse::<i32>().unwrap())))
        .parse_state(input)
}

fn expr_binop<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    let bin_op = token('+').map(|_| BinOp::Plus)
        .or(token('*').map(|_| BinOp::Mult))
        .or(token('-').map(|_| BinOp::Sub));

    (token('('), bin_op, token(' '), parser(expr::<I>), token(' '), parser(expr::<I>), token(')'))
        .map(|p| Expr::BinOp(p.1, Box::new(p.3), Box::new(p.5)))
        .parse_state(input)
}

fn ident<I>(input: State<I>) -> ParseResult<String, I> where I : Stream<Item=char> {
    (letter(), many(alpha_num().or(choice([token('_'), token('-')]))))
        .map(|(c, mut s): (char, String)| { s.insert(0, c); s })
        .parse_state(input)
}

fn expr_lambda<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    (string("(lambda ("), parser(ident::<I>), string(") "), parser(expr::<I>), token(')'))
        .map(|p| Expr::Lambda(p.1, Box::new(p.3)))
        .parse_state(input)
}

fn expr_app<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    (token('('), parser(expr::<I>), token(' '), parser(expr::<I>), token(')'))
        .map(|p| Expr::App(Box::new(p.1), Box::new(p.3)))
        .parse_state(input)
}

fn expr_iszero<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    (string("(zero? "), parser(expr::<I>), token(')'))
        .map(|p| Expr::IsZero(Box::new(p.1)))
        .parse_state(input)
}

fn atom_bool<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    try(string("#t")).map(|_| Expr::Atom(Atom::Boolean(true)))
        .or(try(string("#f")).map(|_| Expr::Atom(Atom::Boolean(false))))
        .parse_state(input)
}

fn expr_if<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    (string("(if "), parser(expr::<I>), token(' '), parser(expr::<I>), token(' '), parser(expr::<I>), token(')'))
        .map(|p| Expr::If(Box::new(p.1), Box::new(p.3), Box::new(p.5)))
        .parse_state(input)
}

fn expr_var<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    parser(ident::<I>)
        .map(|s| Expr::Var(s))
        .parse_state(input)
}

fn expr<I>(input: State<I>) -> ParseResult<Expr, I> where I: Stream<Item=char> {
    // TODO: whitespace per spec
    try(parser(atom_int::<I>))
        .or(try(parser(atom_bool::<I>)))
        .or(try(parser(expr_var::<I>)))
        .or(try(parser(expr_binop::<I>)))
        .or(try(parser(expr_iszero::<I>)))
        .or(try(parser(expr_if::<I>)))
        .or(try(parser(expr_lambda::<I>)))
        .or(try(parser(expr_app::<I>)))
        .parse_state(input)
}

pub fn parse<I>(input: I) -> Result<(Expr, I), ParseError<char>> where I: Stream<Item=char> {
    parser(expr::<I>).parse(input)
}

#[test]
fn test_parse() {
    let exprs = vec![
        ("12345", Expr::Atom(Atom::Int(12345i32))),
        ("(+ 5 7)", Expr::BinOp(BinOp::Plus, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(* 5 7)", Expr::BinOp(BinOp::Mult, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(- 5 7)", Expr::BinOp(BinOp::Sub, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(+ (* 5 7) 7)", Expr::BinOp(BinOp::Plus, Box::new(Expr::BinOp(BinOp::Mult, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("(* (+ 5 7) 7)", Expr::BinOp(BinOp::Mult, Box::new(Expr::BinOp(BinOp::Plus, Box::new(Expr::Atom(Atom::Int(5i32))), Box::new(Expr::Atom(Atom::Int(7i32))))), Box::new(Expr::Atom(Atom::Int(7i32))))),
        ("x", Expr::Var("x".to_string())),
        ("x12_34-xdghy", Expr::Var("x12_34-xdghy".to_string())),
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

    ];

    let parse_str = |s| { 
        match parse(s) {
            Ok((v, "")) => Ok(v),
            Err(e) => Err(format!("err parsing {}, err={}", s, e)),
            _ => Err(format!("err parsing {}, some left", s))
        }
    };

    for p in exprs {
        assert_eq!(parse_str(p.0), Ok(p.1));
    }
}
