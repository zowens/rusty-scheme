#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Integer(i32),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Lambda,
    IsZero,
    If,
    True,
    False,
    Whitespace,
    Comment,
}

lexer! {
    fn next_token(text: 'a) -> (Token, &'a str);

    // Whitespace / Commends
    r#"[ \t\r\n]+"# => (Token::Whitespace, text),
    r#"#[|](~(.*[|]#.*))[|]#"# => (Token::Comment, text), // Block
    r#";[^\n]*"# => (Token::Comment, text), // Line

    // Reserved Keywords
    r#"#t"# => (Token::True, text),
    r#"#f"# => (Token::False, text),
    r#"zero?"# => (Token::IsZero, text),
    r#"lambda"# => (Token::Lambda, text),
    r#"if"# => (Token::If, text),

    // Reserved Operators
    r#"\+"# => (Token::Plus, text),
    r#"-"# => (Token::Minus, text),
    r#"\*"# => (Token::Star, text),
    r#"/"# => (Token::Slash, text),

    // Parens
    r#"\("# => (Token::LParen, text),
    r#"\)"# => (Token::RParen, text),

    // Numbers
    r#"[0-9]+"# => {
        (if let Ok(i) = text.parse() {
            Token::Integer(i)
        } else {
            panic!("integer {} is out of range", text)
        }, text)
    },
    // Identifiers
    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => (Token::Ident(text.to_owned()), text),

    r#"."# => panic!("unexpected character: {}", text),
}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer { original: s, remaining: s }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

fn span_in(s: &str, t: &str) -> Span {
    let lo = t.subslice_offset(s);
    Span {
        lo: lo,
        hi: lo + s.len(),
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let tok = if let Some(tok) = next_token(&mut self.remaining) {
                tok
            } else {
                return None
            };
            match tok {
                (Token::Whitespace, _) | (Token::Comment, _) => {
                    continue;
                }
                (tok, span) => {
                    return Some((tok, span_in(span, self.original)));
                }
            }
        }
    }
}

#[test]
pub fn test_lex() {
    let tests = vec![
        ("(+ 5 6)", vec![Token::LParen, Token::Plus, Token::Integer(5), Token::Integer(6), Token::RParen]),
        ("      (+     5      6    )  ; comment      ", vec![Token::LParen, Token::Plus, Token::Integer(5), Token::Integer(6), Token::RParen]),
        ("(* 5 (+ x x))", vec![Token::LParen, Token::Star, Token::Integer(5), Token::LParen, Token::Plus, Token::Ident("x".to_string()), Token::Ident("x".to_string()), Token::RParen, Token::RParen]),
        ("(/ 5 #| foo bar baz |# 6)", vec![Token::LParen, Token::Slash, Token::Integer(5), Token::Integer(6), Token::RParen]),
        ("(lambda (x) x)", vec![Token::LParen, Token::Lambda, Token::LParen, Token::Ident("x".to_string()), Token::RParen, Token::Ident("x".to_string()), Token::RParen]),
        ("(if (zero? 1) 5 6)", vec![Token::LParen, Token::If, Token::LParen, Token::IsZero, Token::Integer(1), Token::RParen, Token::Integer(5), Token::Integer(6), Token::RParen]),
        ("(if (zero? 1) #t #f)", vec![Token::LParen, Token::If, Token::LParen, Token::IsZero, Token::Integer(1), Token::RParen, Token::True, Token::False, Token::RParen]),
    ];

    for tc in tests.iter() {
        assert_eq!(Lexer::new(tc.0).map(|i| i.0).collect::<Vec<Token>>(), tc.1);
    }
}
