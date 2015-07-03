#![feature(plugin, subslice_offset)]
#![plugin(plex)]

mod lang;

fn main() {
    // factorial of 5
    let e = "(((lambda (f) ((lambda (g) (f (lambda (x) ((g g) x)))) (lambda (g) (f (lambda (x) ((g g) x)))))) (lambda (fact) (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) 5)";

    let exp = lang::parser::parse(lang::lex::Lexer::new(e)).unwrap();
    println!("{:?}", lang::interp::interp(exp));
}
