extern crate scheme;

fn main() {
    // factorial of 5
    let e = "(((lambda (f) ((lambda (g) (f (lambda (x) ((g g) x)))) (lambda (g) (f (lambda (x) ((g g) x)))))) (lambda (fact) (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) 5)";

    let exp = scheme::parser::parse(scheme::lex::Lexer::new(e)).unwrap();
    println!("{:?}", scheme::interp::interp(&exp));
}
