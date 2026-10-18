//! An experiment with typechecking, theorem proving, and building a full compiler stack.

#![deny(clippy::all, clippy::pedantic)]

type Result<T> = std::result::Result<T, String>;

macro_rules! err {
    ($text:expr, $span:expr, $($fmt:tt)*) => {
        Err(format!(
            "error at {}: {}\n{}",
            $span.location($text),
            format!($($fmt)*),
            $span.string($text),
        ))
    };
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    fn string(&self, source: &str) -> String {
        source[self.lo..self.hi].to_owned()
    }

    fn location(&self, text: &str) -> String {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!("{row}:{col}")
    }
}

enum CharType {
    OpenParen,
    CloseParen,
    Comment,
    Whitespace,
    Symbol,
}

fn char_type(c: char) -> CharType {
    match c {
        '(' => CharType::OpenParen,
        ')' => CharType::CloseParen,
        '#' => CharType::Comment,
        c if c.is_whitespace() => CharType::Whitespace,
        _ => CharType::Symbol,
    }
}

#[derive(Debug, PartialEq)]
enum TokenType {
    OpenParen,
    CloseParen,
    Identifier,
}

#[derive(Debug, PartialEq)]
struct Token(TokenType, Span);

#[cfg(test)]
fn open_paren(lo: usize, hi: usize) -> Token {
    Token(TokenType::OpenParen, Span { lo, hi })
}

#[cfg(test)]
fn close_paren(lo: usize, hi: usize) -> Token {
    Token(TokenType::CloseParen, Span { lo, hi })
}

#[cfg(test)]
fn identifier(lo: usize, hi: usize) -> Token {
    Token(TokenType::Identifier, Span { lo, hi })
}

fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut cursor = 0;

    while let Some(c) = text[cursor..].chars().next() {
        let lo = cursor;

        let token_type = match char_type(c) {
            CharType::OpenParen => {
                cursor += c.len_utf8();
                TokenType::OpenParen
            }
            CharType::CloseParen => {
                cursor += c.len_utf8();
                TokenType::CloseParen
            }
            CharType::Comment => {
                while let Some(c) = text[cursor..].chars().next() {
                    match c {
                        '\n' => break,
                        _ => cursor += c.len_utf8(),
                    }
                }
                continue;
            }
            CharType::Whitespace => {
                while let Some(c) = text[cursor..].chars().next() {
                    match char_type(c) {
                        CharType::Whitespace => cursor += c.len_utf8(),
                        _ => break,
                    }
                }
                continue;
            }
            CharType::Symbol => {
                while let Some(c) = text[cursor..].chars().next() {
                    match char_type(c) {
                        CharType::Symbol => cursor += c.len_utf8(),
                        _ => break,
                    }
                }
                TokenType::Identifier
            }
        };

        tokens.push(Token(token_type, Span { lo, hi: cursor }));
    }

    tokens
}

#[test]
fn test_tokenize() {
    fn run_test(text: &str, expected_tokens: &[Token]) {
        let actual_tokens = tokenize(text);
        assert_eq!(expected_tokens, actual_tokens);
    }

    run_test(" ", &[]);
    run_test("  ", &[]);
    run_test(" # ", &[]);
    run_test("a", &[identifier(0, 1)]);
    run_test(" a", &[identifier(1, 2)]);
    run_test(" a ", &[identifier(1, 2)]);
    run_test(" ab ", &[identifier(1, 3)]);
    run_test(" a b ", &[identifier(1, 2), identifier(3, 4)]);
    run_test(" a#b ", &[identifier(1, 2)]);
    run_test("()", &[open_paren(0, 1), close_paren(1, 2)]);
}

#[derive(Debug, PartialEq)]
enum Sexp {
    Atom(Span),
    List(Span, Vec<Sexp>),
}

#[cfg(test)]
fn atom(lo: usize, hi: usize) -> Sexp {
    Sexp::Atom(Span { lo, hi })
}

#[cfg(test)]
fn list(lo: usize, hi: usize, list: Vec<Sexp>) -> Sexp {
    Sexp::List(Span { lo, hi }, list)
}

fn sexpify(tokens: &[Token], text: &str) -> Result<Vec<Sexp>> {
    let mut stack = Vec::new();
    let mut level = Vec::new();

    for Token(token_type, span) in tokens {
        match token_type {
            TokenType::OpenParen => {
                stack.push((span, level));
                level = Vec::new();
            }
            TokenType::CloseParen => {
                let Some((left_span, prev_level)) = stack.pop() else {
                    return err!(text, span, "extra close paren");
                };

                let list = level;
                level = prev_level;

                let span = Span {
                    lo: left_span.lo,
                    hi: span.hi,
                };

                level.push(Sexp::List(span, list));
            }
            TokenType::Identifier => level.push(Sexp::Atom(*span)),
        }
    }

    match stack.pop() {
        None => Ok(level),
        Some((span, _)) => err!(text, span, "unclosed open paren"),
    }
}

#[test]
fn test_sexpify() -> Result<()> {
    fn run_test(text: &str, expected_sexps: &[Sexp]) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexps = sexpify(&tokens, text)?;
        assert_eq!(expected_sexps, actual_sexps);
        Ok(())
    }

    run_test("", &[])?;
    run_test("a", &[atom(0, 1)])?;
    run_test("a b", &[atom(0, 1), atom(2, 3)])?;
    run_test("()", &[list(0, 2, vec![])])?;
    assert_eq!(
        run_test("(", &[]),
        Err(String::from("error at 1:1: unclosed open paren\n("))
    );
    assert_eq!(
        run_test(")", &[]),
        Err(String::from("error at 1:1: extra close paren\n)"))
    );
    run_test(
        "(a)(b()c(d(e(f)g)h)i)j lm",
        &[
            list(0, 3, vec![atom(1, 2)]),
            list(
                3,
                21,
                vec![
                    atom(4, 5),
                    list(5, 7, vec![]),
                    atom(7, 8),
                    list(
                        8,
                        19,
                        vec![
                            atom(9, 10),
                            list(
                                10,
                                17,
                                vec![atom(11, 12), list(12, 15, vec![atom(13, 14)]), atom(15, 16)],
                            ),
                            atom(17, 18),
                        ],
                    ),
                    atom(19, 20),
                ],
            ),
            atom(21, 22),
            atom(23, 25),
        ],
    )?;
    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
enum Ast {
    Var(Span, String),
    App(Span, Box<Ast>, Box<Ast>),
    Fun {
        span: Span,
        var: String,
        arg: Box<Ast>,
        ret: Box<Ast>,
    },
}

#[cfg(test)]
fn var(lo: usize, hi: usize, var: &str) -> Ast {
    Ast::Var(Span { lo, hi }, var.to_owned())
}

#[cfg(test)]
fn app(lo: usize, hi: usize, f: Ast, x: Ast) -> Ast {
    Ast::App(Span { lo, hi }, Box::new(f), Box::new(x))
}

#[cfg(test)]
fn fun(lo: usize, hi: usize, var: &str, arg: Ast, ret: Ast) -> Ast {
    Ast::Fun {
        span: Span { lo, hi },
        var: var.to_owned(),
        arg: Box::new(arg),
        ret: Box::new(ret),
    }
}

fn parse(sexp: &Sexp, text: &str) -> Result<Ast> {
    Ok(match sexp {
        Sexp::Atom(span) => Ast::Var(*span, span.string(text)),
        Sexp::List(span, list) => match &list[..] {
            [f, x] => Ast::App(*span, Box::new(parse(f, text)?), Box::new(parse(x, text)?)),
            [
                Sexp::Atom(var),
                Sexp::Atom(colon),
                arg,
                Sexp::Atom(arrow),
                ret,
            ] => {
                if colon.string(text) != ":" {
                    return err!(text, span, "expected colon");
                }
                if arrow.string(text) != "->" {
                    return err!(text, span, "expected arrow");
                }
                Ast::Fun {
                    span: *span,
                    var: var.string(text),
                    arg: Box::new(parse(arg, text)?),
                    ret: Box::new(parse(ret, text)?),
                }
            }
            _ => return err!(text, span, "expected application or abstraction"),
        },
    })
}

#[test]
fn test_parse() -> Result<()> {
    fn run_test(text: &str, expected_asts: &[Ast]) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexps = sexpify(&tokens, text)?;
        let actual_asts = actual_sexps
            .into_iter()
            .map(|sexp| parse(&sexp, text))
            .collect::<Result<Vec<_>>>()?;
        assert_eq!(expected_asts, actual_asts);
        Ok(())
    }

    run_test("a", &[var(0, 1, "a")])?;
    run_test("(f x)", &[app(0, 5, var(1, 2, "f"), var(3, 4, "x"))])?;
    run_test(
        "(x : t -> y)",
        &[fun(0, 12, "x", var(5, 6, "t"), var(10, 11, "y"))],
    )?;
    assert_eq!(
        run_test("(x y z)", &[]),
        Err(String::from(
            "error at 1:1: expected application or abstraction\n(x y z)"
        ))
    );
    Ok(())
}

fn substitute(key: &str, val: &Ast, ast: Ast) -> Ast {
    match ast {
        Ast::Var(_span, var) if var == key => val.clone(),
        Ast::Var(_span, _) => ast,
        Ast::App(span, f, x) => Ast::App(
            span,
            Box::new(substitute(key, val, *f)),
            Box::new(substitute(key, val, *x)),
        ),
        Ast::Fun {
            span,
            var,
            arg,
            ret,
        } => Ast::Fun {
            span,
            var,
            arg: Box::new(substitute(key, val, *arg)),
            ret: Box::new(substitute(key, val, *ret)),
        },
    }
}

fn reduce(ast: Ast) -> Ast {
    match ast {
        Ast::Var(span, var) => Ast::Var(span, var),
        Ast::App(span, f, x) => match *f {
            Ast::Fun {
                span: _,
                var,
                arg: _,
                ret,
            } => substitute(&var, &x, *ret),
            f => Ast::App(span, Box::new(f), x),
        },
        Ast::Fun {
            span,
            var,
            arg,
            ret,
        } => Ast::Fun {
            span,
            var,
            arg: Box::new(reduce(*arg)),
            ret: Box::new(reduce(*ret)),
        },
    }
}

// TODO: test `reduce`

fn run(text: &str) -> Result<()> {
    let tokens = tokenize(text);
    let sexps = sexpify(&tokens, text)?;
    let asts = sexps
        .into_iter()
        .map(|sexp| parse(&sexp, text))
        .map(|result| result.map(reduce))
        .collect::<Result<Vec<_>>>()?;
    println!("{asts:#?}");
    Ok(())
}

fn main() -> std::process::ExitCode {
    match (|| {
        let args: Vec<_> = std::env::args().collect();
        let name = args.get(1).ok_or("usage: pass a .iz file")?.clone();
        let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
        run(&text)
    })() {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            std::process::ExitCode::FAILURE
        }
    }
}
