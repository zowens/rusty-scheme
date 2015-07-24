use repllib::{Repl,ReplEnv};
use std::fmt;

pub fn run() {
    let env = SchemeReplEnv { prompt: "> ".to_string() };
    let r = SchemeRepl;
    r._loop(&env);
}

struct SchemeRepl;

struct SchemeReplEnv {
    pub prompt: String
}

impl ReplEnv for SchemeReplEnv {
    fn preamble(&self) -> bool {
        true
    }

    fn colorize(&self) -> bool {
        false
    }

    fn prompt(&self) -> &String {
        &self.prompt
    }
}

enum Cmd {
    Expression(super::Expr),
    EvalResult(super::Value),
    Exit,
    Nil,
}

enum ReplErr {
    ReadErr(&'static str),
    // TODO: evaluation error
}

impl fmt::Display for ReplErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ReplErr::ReadErr(ref x) => write!(f, "invalid expression: {}", x),
        }
    }
}

type ReplResult = Result<Cmd,ReplErr>;

impl Repl<Cmd,ReplErr,SchemeReplEnv> for SchemeRepl {
    fn preamble(&self, _: &SchemeReplEnv) -> &SchemeRepl { 
        println!("Pico Scheme\n:quit to exit");
        self 
    }

    fn read(&self, input: String, _: &SchemeReplEnv) -> ReplResult {
        match &input[..] {
            ":quit" => Ok(Cmd::Exit),
            _       => match super::parser::parse(super::lex::Lexer::new(&input)) {
                Ok(exp) => Ok(Cmd::Expression(exp)),
                Err((_, e))  => Err(ReplErr::ReadErr(e)),
            }
        }
    }

    fn eval(&self, cmd: Cmd, _: &SchemeReplEnv) -> ReplResult {
        match cmd {
            Cmd::Exit => Ok(Cmd::Exit),
            Cmd::Expression(e) => Ok(Cmd::EvalResult(super::interp::interp(&e))),
            _ => unreachable!(),
        }
    }

    fn print(&self, cmd: Cmd, _: &SchemeReplEnv) -> ReplResult {
        match cmd {
            Cmd::Exit           => Ok(Cmd::Exit),
            Cmd::EvalResult(r)  => {
                println!("{}", r);
                Ok(Cmd::Nil)
            },
            _                   => unreachable!()
        }
    }

    fn break_loop(&self, cmd: &Cmd, _: &SchemeReplEnv) -> bool {
        if let Cmd::Exit = *cmd {
            true
        } else {
            false
        }
    }
}
