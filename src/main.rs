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
    fn string<'a>(&self, text: &'a str) -> &'a str {
        &text[self.lo..self.hi]
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
    Type(Span),
    Prop(Span),
    Var(Span),
    App(Span, Box<Ast>, Box<Ast>),
    Lambda(Span, Span, Box<Ast>, Box<Ast>),
    Forall(Span, Span, Box<Ast>, Box<Ast>),
}

#[cfg(test)]
fn typ(lo: usize, hi: usize) -> Ast {
    Ast::Type(Span { lo, hi })
}

#[cfg(test)]
fn prop(lo: usize, hi: usize) -> Ast {
    Ast::Prop(Span { lo, hi })
}

#[cfg(test)]
fn var(lo: usize, hi: usize) -> Ast {
    Ast::Var(Span { lo, hi })
}

#[cfg(test)]
fn app(lo: usize, hi: usize, f: Ast, x: Ast) -> Ast {
    Ast::App(Span { lo, hi }, Box::new(f), Box::new(x))
}

#[cfg(test)]
fn lambda((lo, hi): (usize, usize), (var_lo, var_hi): (usize, usize), arg: Ast, ret: Ast) -> Ast {
    Ast::Lambda(
        Span { lo, hi },
        Span {
            lo: var_lo,
            hi: var_hi,
        },
        Box::new(arg),
        Box::new(ret),
    )
}

#[cfg(test)]
fn forall((lo, hi): (usize, usize), (var_lo, var_hi): (usize, usize), arg: Ast, ret: Ast) -> Ast {
    Ast::Forall(
        Span { lo, hi },
        Span {
            lo: var_lo,
            hi: var_hi,
        },
        Box::new(arg),
        Box::new(ret),
    )
}

fn parse(sexp: &Sexp, text: &str) -> Result<Ast> {
    Ok(match sexp {
        Sexp::Atom(span) => match span.string(text) {
            "T" => Ast::Type(*span),
            "P" => Ast::Prop(*span),
            _ => Ast::Var(*span),
        },
        Sexp::List(span, list) => match &list[..] {
            [f, x] => Ast::App(*span, Box::new(parse(f, text)?), Box::new(parse(x, text)?)),
            [
                Sexp::Atom(op),
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
                let arg = Box::new(parse(arg, text)?);
                let ret = Box::new(parse(ret, text)?);
                match op.string(text) {
                    "lambda" => Ast::Lambda(*span, *var, arg, ret),
                    "forall" => Ast::Forall(*span, *var, arg, ret),
                    _ => return err!(text, op, "expected lambda or forall"),
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

    run_test("a", &[var(0, 1)])?;
    run_test("(f x)", &[app(0, 5, var(1, 2), var(3, 4))])?;
    run_test(
        "(lambda x : (forall A : T -> P) -> y)",
        &[lambda(
            (0, 37),
            (8, 9),
            forall((12, 31), (20, 21), typ(24, 25), prop(29, 30)),
            var(35, 36),
        )],
    )?;
    assert_eq!(
        run_test("(x y z)", &[]),
        Err(String::from(
            "error at 1:1: expected application or abstraction\n(x y z)"
        ))
    );
    Ok(())
}

impl Ast {
    fn substitute(self, text: &str, key: &str, val: &Ast) -> Ast {
        match self {
            Ast::Type(_) | Ast::Prop(_) => self,
            Ast::Var(var) => {
                if var.string(text) == key {
                    val.clone()
                } else {
                    self
                }
            }
            Ast::App(span, f, x) => Ast::App(
                span,
                Box::new(f.substitute(text, key, val)),
                Box::new(x.substitute(text, key, val)),
            ),
            Ast::Lambda(span, var, arg, ret) => {
                let arg = Box::new(arg.substitute(text, key, val));
                let ret = if var.string(text) == key {
                    ret
                } else {
                    Box::new(ret.substitute(text, key, val))
                };
                Ast::Lambda(span, var, arg, ret)
            }
            Ast::Forall(span, var, arg, ret) => {
                let arg = Box::new(arg.substitute(text, key, val));
                let ret = if var.string(text) == key {
                    ret
                } else {
                    Box::new(ret.substitute(text, key, val))
                };
                Ast::Forall(span, var, arg, ret)
            }
        }
    }

    fn beta_reduce(self, text: &str) -> Ast {
        match self {
            Ast::Type(_) | Ast::Prop(_) | Ast::Var(_) => self,
            Ast::App(span, f, x) => {
                let f = f.beta_reduce(text);
                let x = x.beta_reduce(text);
                match f {
                    Ast::Lambda(_, var, _, ret) => {
                        ret.substitute(text, var.string(text), &x).beta_reduce(text)
                    }
                    _ => Ast::App(span, Box::new(f), Box::new(x)),
                }
            }
            Ast::Lambda(span, var, arg, ret) => Ast::Lambda(
                span,
                var,
                Box::new(arg.beta_reduce(text)),
                Box::new(ret.beta_reduce(text)),
            ),
            Ast::Forall(span, var, arg, ret) => Ast::Forall(
                span,
                var,
                Box::new(arg.beta_reduce(text)),
                Box::new(ret.beta_reduce(text)),
            ),
        }
    }

    fn is_beta_equivalent(text: &str, lhs: Ast, rhs: Ast) -> bool {
        match (lhs.beta_reduce(text), rhs.beta_reduce(text)) {
            (Ast::Type(_), Ast::Type(_)) | (Ast::Prop(_), Ast::Prop(_)) => true,

            (Ast::Var(lhs_var), Ast::Var(rhs_var)) => lhs_var.string(text) == rhs_var.string(text),

            (Ast::App(_, lhs_f, lhs_x), Ast::App(_, rhs_f, rhs_x)) => {
                Self::is_beta_equivalent(text, *lhs_f, *rhs_f)
                    && Self::is_beta_equivalent(text, *lhs_x, *rhs_x)
            }

            (
                Ast::Lambda(_, lhs_var, lhs_arg, lhs_ret),
                Ast::Lambda(_, rhs_var, rhs_arg, rhs_ret),
            )
            | (
                Ast::Forall(_, lhs_var, lhs_arg, lhs_ret),
                Ast::Forall(_, rhs_var, rhs_arg, rhs_ret),
            ) => {
                Self::is_beta_equivalent(text, *lhs_arg, *rhs_arg)
                    && Self::is_beta_equivalent(
                        text,
                        *lhs_ret,
                        rhs_ret.substitute(text, rhs_var.string(text), &Ast::Var(lhs_var)),
                    )
            }

            _ => false,
        }
    }

    fn typecheck_(self, text: &str, context: &mut Vec<(Span, Ast)>) -> Result<Ast> {
        Ok(match self {
            Ast::Type(span) => return err!(text, span, "no type for T"),
            Ast::Prop(span) => Ast::Type(span),
            Ast::Var(var) => {
                match context
                    .iter()
                    .find(|(key, _)| var.string(text) == key.string(text))
                {
                    Some((_, val)) => val.clone(),
                    None => return err!(text, var, "no type for variable"),
                }
            }
            Ast::App(span, f, x) => {
                let f_type = f.typecheck_(text, context)?;
                let x_type = x.clone().typecheck_(text, context)?;
                match f_type {
                    Ast::Forall(_, var, arg, ret)
                        if Self::is_beta_equivalent(text, *arg.clone(), x_type.clone()) =>
                    {
                        ret.substitute(text, var.string(text), &x)
                    }
                    _ => Ast::App(span, Box::new(f_type), Box::new(x_type)),
                }
            }
            Ast::Lambda(span, var, arg, ret) => {
                context.push((var, *arg.clone()));
                Ast::Forall(span, var, arg, Box::new(ret.typecheck_(text, context)?))
            }
            Ast::Forall(_, var, arg, ret) => {
                context.push((var, *arg));
                ret.typecheck_(text, context)?
            }
        })
    }

    fn typecheck(self, text: &str) -> Result<Ast> {
        self.typecheck_(text, &mut Vec::new())
    }
}

// TODO: test `Ast` functions

fn run(text: &str) -> Result<()> {
    let tokens = tokenize(text);
    let sexps = sexpify(&tokens, text)?;
    let asts = sexps
        .into_iter()
        .map(|sexp| parse(&sexp, text))
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
