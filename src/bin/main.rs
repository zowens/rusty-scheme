extern crate scheme;

fn main() {
    // factorial of 5
    let e = "(letrec ((fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) (fact 5))";

    let exp = scheme::parser::parse(scheme::lex::Lexer::new(e)).unwrap();
    println!("{:?}", scheme::interp::interp(&exp));
}
