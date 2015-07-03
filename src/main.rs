#![feature(plugin, subslice_offset)]
#![plugin(plex)]
extern crate parser_combinators;

mod lang;
use lang::*;

fn main() {
    // factorial of 5
    let e = parser::parse("(((lambda (f) ((lambda (g) (f (lambda (x) ((g g) x)))) (lambda (g) (f (lambda (x) ((g g) x)))))) (lambda (fact) (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))) 5)").unwrap().0;

    println!("{:?}", interp::interp(e));
}
