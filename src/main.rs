//! An experiment with typechecking, theorem proving, and building a full compiler stack.

#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::too_many_lines)]

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
                    cursor += c.len_utf8();
                    if c == '\n' {
                        break;
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
enum Expr {
    Type(Span),
    Var(Span),
    App(Span, Box<Expr>, Box<Expr>),
    Lambda(Span, Span, Box<Expr>, Box<Expr>),
    Forall(Span, Span, Box<Expr>, Box<Expr>),
}

fn parse_expr(sexp: &Sexp, text: &str) -> Result<Expr> {
    Ok(match sexp {
        Sexp::Atom(span) => match span.string(text) {
            "*" => Expr::Type(*span),
            _ => Expr::Var(*span),
        },
        Sexp::List(span, list) => match &list[..] {
            [f, x] => Expr::App(
                *span,
                Box::new(parse_expr(f, text)?),
                Box::new(parse_expr(x, text)?),
            ),
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
                let arg = Box::new(parse_expr(arg, text)?);
                let ret = Box::new(parse_expr(ret, text)?);
                match op.string(text) {
                    "lambda" => Expr::Lambda(*span, *var, arg, ret),
                    "forall" => Expr::Forall(*span, *var, arg, ret),
                    _ => return err!(text, op, "expected lambda or forall"),
                }
            }
            [
                Sexp::Atom(let_),
                Sexp::Atom(var),
                Sexp::Atom(colon),
                arg,
                Sexp::Atom(equals),
                val,
                Sexp::Atom(in_),
                ret,
            ] => {
                if let_.string(text) != "let" {
                    return err!(text, span, "expected let");
                }
                if colon.string(text) != ":" {
                    return err!(text, span, "expected colon");
                }
                if equals.string(text) != "=" {
                    return err!(text, span, "expected equals");
                }
                if in_.string(text) != "in" {
                    return err!(text, span, "expected in");
                }
                let arg = Box::new(parse_expr(arg, text)?);
                let ret = Box::new(parse_expr(ret, text)?);
                let val = Box::new(parse_expr(val, text)?);
                Expr::App(*span, Box::new(Expr::Lambda(*span, *var, arg, ret)), val)
            }
            _ => {
                return err!(
                    text,
                    span,
                    "expected application, abstraction, or let binding"
                );
            }
        },
    })
}

#[derive(Clone, Debug, PartialEq)]
struct Decl(Span, Span, Box<Expr>, Box<Expr>);

fn parse_decl(sexp: &Sexp, text: &str) -> Result<Decl> {
    Ok(match sexp {
        Sexp::Atom(span) => return err!(text, span, "expected declaration"),
        Sexp::List(span, list) => match &list[..] {
            [
                Sexp::Atom(var),
                Sexp::Atom(colon),
                arg,
                Sexp::Atom(equals),
                val,
            ] => {
                if colon.string(text) != ":" {
                    return err!(text, span, "expected colon");
                }
                if equals.string(text) != "=" {
                    return err!(text, span, "expected equals");
                }
                Decl(
                    *span,
                    *var,
                    Box::new(parse_expr(arg, text)?),
                    Box::new(parse_expr(val, text)?),
                )
            }
            _ => return err!(text, span, "expected declaration"),
        },
    })
}

#[test]
fn test_parse() -> Result<()> {
    fn type_(lo: usize, hi: usize) -> Expr {
        Expr::Type(Span { lo, hi })
    }
    fn var(lo: usize, hi: usize) -> Expr {
        Expr::Var(Span { lo, hi })
    }
    fn app((lo, hi): (usize, usize), f: Expr, x: Expr) -> Expr {
        Expr::App(Span { lo, hi }, Box::new(f), Box::new(x))
    }
    fn lambda(
        (lo, hi): (usize, usize),
        (var_lo, var_hi): (usize, usize),
        arg: Expr,
        ret: Expr,
    ) -> Expr {
        Expr::Lambda(
            Span { lo, hi },
            Span {
                lo: var_lo,
                hi: var_hi,
            },
            Box::new(arg),
            Box::new(ret),
        )
    }
    fn forall(
        (lo, hi): (usize, usize),
        (var_lo, var_hi): (usize, usize),
        arg: Expr,
        ret: Expr,
    ) -> Expr {
        Expr::Forall(
            Span { lo, hi },
            Span {
                lo: var_lo,
                hi: var_hi,
            },
            Box::new(arg),
            Box::new(ret),
        )
    }
    fn decl(
        (lo, hi): (usize, usize),
        (var_lo, var_hi): (usize, usize),
        arg: Expr,
        val: Expr,
    ) -> Decl {
        Decl(
            Span { lo, hi },
            Span {
                lo: var_lo,
                hi: var_hi,
            },
            Box::new(arg),
            Box::new(val),
        )
    }

    fn run_test_expr(text: &str, expected_exprs: &[Expr]) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexps = sexpify(&tokens, text)?;
        let actual_exprs = actual_sexps
            .into_iter()
            .map(|sexp| parse_expr(&sexp, text))
            .collect::<Result<Vec<_>>>()?;
        assert_eq!(expected_exprs, actual_exprs);
        Ok(())
    }

    fn run_test_decl(text: &str, expected_decls: &[Decl]) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexps = sexpify(&tokens, text)?;
        let actual_decls = actual_sexps
            .into_iter()
            .map(|sexp| parse_decl(&sexp, text))
            .collect::<Result<Vec<_>>>()?;
        assert_eq!(expected_decls, actual_decls);
        Ok(())
    }

    run_test_expr("*", &[type_(0, 1)])?;
    run_test_expr("abc", &[var(0, 3)])?;
    run_test_expr("(f x)", &[app((0, 5), var(1, 2), var(3, 4))])?;
    run_test_expr(
        "(lambda x : (forall A : * -> A) -> y)",
        &[lambda(
            (0, 37),
            (8, 9),
            forall((12, 31), (20, 21), type_(24, 25), var(29, 30)),
            var(35, 36),
        )],
    )?;
    run_test_expr(
        "(let x : t = y in z)",
        &[app(
            (0, 20),
            lambda((0, 20), (5, 6), var(9, 10), var(18, 19)),
            var(13, 14),
        )],
    )?;
    assert_eq!(
        run_test_expr("(x y z)", &[]),
        Err(String::from(
            "error at 1:1: expected application, abstraction, or let binding\n(x y z)"
        ))
    );

    run_test_decl(
        "(x : * = P) (y : P = (forall z : P -> z))",
        &[
            decl((0, 11), (1, 2), type_(5, 6), var(9, 10)),
            decl(
                (12, 41),
                (13, 14),
                var(17, 18),
                forall((21, 40), (29, 30), var(33, 34), var(38, 39)),
            ),
        ],
    )?;

    Ok(())
}

#[derive(Clone)]
struct State<'a> {
    text: &'a str,
    context: Vec<(&'a str, Option<Expr>)>,
}

impl State<'_> {
    fn new(text: &str) -> State<'_> {
        let context = Vec::new();
        State { text, context }
    }

    fn bind(&mut self, var: Span) {
        self.context.push((var.string(self.text), None));
    }

    fn subst(&mut self, from: Span, to: &Expr) {
        self.context
            .push((from.string(self.text), Some(to.clone())));
    }

    fn pop(&mut self) {
        self.context.pop();
    }

    fn lookup(&self, var: Span) -> Expr {
        let var_ = var.string(self.text);
        for (key, val) in &self.context {
            if var_ == *key {
                return match val {
                    None => Expr::Var(var),
                    Some(val) => val.clone(),
                };
            }
        }
        Expr::Var(var)
    }
}

impl Expr {
    fn beta_reduce(&self, state: &mut State) -> Expr {
        match self {
            Expr::Type(span) => Expr::Type(*span),
            Expr::Var(var) => state.lookup(*var),
            Expr::App(span, f, x) => {
                let f = f.beta_reduce(state);
                let x = x.beta_reduce(state);
                match f {
                    Expr::Lambda(_, var, _, ret) => {
                        state.subst(var, &x);
                        let ret = ret.beta_reduce(state);
                        state.pop();
                        ret
                    }
                    _ => Expr::App(*span, Box::new(f), Box::new(x)),
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state);
                state.bind(*var);
                let ret = ret.beta_reduce(state);
                state.pop();
                Expr::Lambda(*span, *var, Box::new(arg), Box::new(ret))
            }
            Expr::Forall(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state);
                state.bind(*var);
                let ret = ret.beta_reduce(state);
                state.pop();
                Expr::Forall(*span, *var, Box::new(arg), Box::new(ret))
            }
        }
    }

    fn is_beta_eq(lhs: &Expr, lhs_state: &mut State, rhs: &Expr, rhs_state: &mut State) -> bool {
        match (lhs.beta_reduce(lhs_state), rhs.beta_reduce(rhs_state)) {
            (Expr::Type(_), Expr::Type(_)) => true,

            (Expr::Var(lhs_var), Expr::Var(rhs_var)) => {
                lhs_var.string(lhs_state.text) == rhs_var.string(rhs_state.text)
            }

            (Expr::App(_, lhs_f, lhs_x), Expr::App(_, rhs_f, rhs_x)) => {
                Self::is_beta_eq(&lhs_f, lhs_state, &rhs_f, rhs_state)
                    && Self::is_beta_eq(&lhs_x, lhs_state, &rhs_x, rhs_state)
            }

            (
                Expr::Lambda(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Lambda(_, rhs_var, rhs_arg, rhs_ret),
            )
            | (
                Expr::Forall(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Forall(_, rhs_var, rhs_arg, rhs_ret),
            ) => {
                let args_eq = Self::is_beta_eq(&lhs_arg, lhs_state, &rhs_arg, rhs_state);
                rhs_state.subst(rhs_var, &Expr::Var(lhs_var));
                let rets_eq = Self::is_beta_eq(&lhs_ret, lhs_state, &rhs_ret, rhs_state);
                rhs_state.pop();
                args_eq && rets_eq
            }

            _ => false,
        }
    }

    fn typecheck(&self, state: &mut State) -> Result<Expr> {
        Ok(match self {
            Expr::Type(span) => return err!(state.text, span, "no type for *"),
            Expr::Var(var) => state.lookup(*var),
            Expr::App(span, f, x) => {
                let f_type = f.typecheck(state)?;
                let x_type = x.typecheck(state)?;
                match f_type {
                    Expr::Forall(_, var, arg, ret) => {
                        if Self::is_beta_eq(&arg, &mut state.clone(), &x_type, state) {
                            state.subst(var, x);
                            let ret = ret.beta_reduce(state);
                            state.pop();
                            ret
                        } else {
                            return err!(state.text, span, "wrong type for lambda argument");
                        }
                    }
                    _ => return err!(state.text, span, "expected forall type"),
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                state.subst(*var, arg);
                let ret = ret.typecheck(state)?;
                state.pop();
                Expr::Forall(*span, *var, arg.clone(), Box::new(ret))
            }
            Expr::Forall(_, var, arg, ret) => {
                state.subst(*var, arg);
                let ret = ret.typecheck(state)?;
                state.pop();
                ret
            }
        })
    }

    fn to_string(&self, text: &str) -> String {
        match self {
            Expr::Type(_) => String::from("*"),
            Expr::Var(var) => String::from(var.string(text)),
            Expr::App(_, f, x) => format!("({} {})", f.to_string(text), x.to_string(text)),
            Expr::Lambda(_, var, arg, ret) => format!(
                "(lambda {} : {} -> {})",
                var.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
            Expr::Forall(_, var, arg, ret) => format!(
                "(forall {} : {} -> {})",
                var.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
        }
    }
}

#[test]
fn test_typecheck() -> Result<()> {
    fn run_test(text: &str, expected_eval: &str, expected_type: &str) -> Result<()> {
        let tokens = tokenize(text);
        let sexp = sexpify(&tokens, text)?.remove(0);
        let expr = parse_expr(&sexp, text)?;
        let actual_eval = expr.beta_reduce(&mut State::new(text)).to_string(text);
        let actual_type = expr.typecheck(&mut State::new(text))?;
        assert_eq!(expected_eval, actual_eval);
        assert_eq!(expected_type, actual_type.to_string(text));
        Ok(())
    }

    assert_eq!(
        run_test("*", "", ""),
        Err(String::from("error at 1:1: no type for *\n*"))
    );
    run_test("P", "P", "P")?;
    run_test("(forall x : P -> x)", "(forall x : P -> x)", "P")?;
    run_test(
        "(lambda x : P -> x)",
        "(lambda x : P -> x)",
        "(forall x : P -> P)",
    )?;
    run_test(
        "((lambda x : P -> x) (forall x : P -> x))",
        "(forall x : P -> x)",
        "P",
    )?;
    run_test(
        "(let x : P = (forall x : P -> x) in x)",
        "(forall x : P -> x)",
        "P",
    )?;
    Ok(())
}

fn run(text: &str) -> Result<()> {
    let tokens = tokenize(text);
    let sexps = sexpify(&tokens, text)?;
    let decls = sexps
        .into_iter()
        .map(|sexp| parse_decl(&sexp, text))
        .collect::<Result<Vec<_>>>()?;

    let mut state = State::new(text);
    for Decl(span, var, arg, val) in decls {
        let given = arg.beta_reduce(&mut state);
        let inferred = val.typecheck(&mut state)?;
        if Expr::is_beta_eq(&given, &mut state.clone(), &inferred, &mut state) {
            state.subst(var, &val);
        } else {
            let given = given.to_string(state.text);
            let inferred = inferred.to_string(state.text);
            return err!(
                state.text,
                span,
                "was given {given} but inferred {inferred}"
            );
        }
    }

    Ok(())
}

fn main() -> std::process::ExitCode {
    match (|| {
        let args: Vec<_> = std::env::args().collect();
        let name = args.get(1).ok_or("usage: pass a .iz file")?;
        let text = std::fs::read_to_string(name).map_err(|_| format!("could not read {name}"))?;
        run(&text)
    })() {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            std::process::ExitCode::FAILURE
        }
    }
}
