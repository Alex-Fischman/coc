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

#[derive(Clone, Copy, PartialEq)]
struct Span {
    lo: usize,
    hi: usize,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo, self.hi)
    }
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

impl Sexp {
    fn span(&self) -> Span {
        match self {
            Sexp::Atom(span) | Sexp::List(span, ..) => *span,
        }
    }
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

impl Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Type(span)
            | Expr::Var(span)
            | Expr::App(span, ..)
            | Expr::Lambda(span, ..)
            | Expr::Forall(span, ..) => *span,
        }
    }

    fn to_string(&self, text: &str) -> String {
        match self {
            Expr::Type(_) => String::from("*"),
            Expr::Var(var) => String::from(var.string(text)),
            Expr::App(_, f, x) => format!("({} {})", f.to_string(text), x.to_string(text)),
            Expr::Lambda(_, var, arg, ret) => format!(
                "(({} : {}) -> {})",
                var.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
            Expr::Forall(_, var, arg, ret) => format!(
                "(({} : {}) => {})",
                var.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
        }
    }
}

fn parse_expr(text: &str, sexp: &Sexp) -> Result<Expr> {
    Ok(match sexp {
        Sexp::Atom(span) => match span.string(text) {
            "*" => Expr::Type(*span),
            _ => Expr::Var(*span),
        },
        Sexp::List(span, list) => {
            let mut init_groups = Vec::new();
            let mut last_group = Vec::new();
            for sexp in list {
                match sexp {
                    Sexp::Atom(span) if span.string(text) == "->" => {
                        init_groups.push((true, last_group));
                        last_group = Vec::new();
                    }
                    Sexp::Atom(span) if span.string(text) == "=>" => {
                        init_groups.push((false, last_group));
                        last_group = Vec::new();
                    }
                    _ => last_group.push(sexp),
                }
            }

            let Some(mut ret) = last_group
                .into_iter()
                .map(|sexp| parse_expr(text, sexp))
                .reduce(|f, x| match (f, x) {
                    (Err(err), _) | (_, Err(err)) => Err(err),
                    (Ok(f), Ok(x)) => {
                        let span = Span {
                            lo: f.span().lo,
                            hi: x.span().hi,
                        };
                        Ok(Expr::App(span, Box::new(f), Box::new(x)))
                    }
                })
                .transpose()?
            else {
                return err!(text, span, "expected expression");
            };

            while let Some((is_lambda, mut bindings)) = init_groups.pop() {
                while let Some(binding) = bindings.pop() {
                    let (var, arg) = match binding {
                        Sexp::Atom(span) => return err!(text, span, "expected binding"),
                        Sexp::List(span, list) => match &list[..] {
                            [Sexp::Atom(var), Sexp::Atom(colon), arg] => {
                                if colon.string(text) != ":" {
                                    return err!(text, span, "expected colon");
                                }
                                (var, Box::new(parse_expr(text, arg)?))
                            }
                            _ => return err!(text, span, "expected binding"),
                        },
                    };

                    let span = if init_groups.is_empty() && bindings.is_empty() {
                        *span
                    } else {
                        Span {
                            lo: binding.span().lo,
                            hi: ret.span().hi,
                        }
                    };

                    ret = if is_lambda {
                        Expr::Lambda(span, *var, arg, Box::new(ret))
                    } else {
                        Expr::Forall(span, *var, arg, Box::new(ret))
                    };
                }
            }

            ret
        }
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
                    Box::new(parse_expr(text, arg)?),
                    Box::new(parse_expr(text, val)?),
                )
            }
            _ => return err!(text, span, "expected declaration"),
        },
    })
}

#[test]
fn test_parse() -> Result<()> {
    fn star(lo: usize, hi: usize) -> Expr {
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
            .map(|sexp| parse_expr(text, &sexp))
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

    run_test_expr("*", &[star(0, 1)])?;
    run_test_expr("abc", &[var(0, 3)])?;
    run_test_expr("(f x)", &[app((1, 4), var(1, 2), var(3, 4))])?;
    run_test_expr(
        "((x : ((A : *) => A)) -> y)",
        &[lambda(
            (0, 27),
            (2, 3),
            forall((6, 20), (8, 9), star(12, 13), var(18, 19)),
            var(25, 26),
        )],
    )?;
    run_test_expr(
        "(f x y)",
        &[app((1, 6), app((1, 4), var(1, 2), var(3, 4)), var(5, 6))],
    )?;
    run_test_expr(
        "((T : *) => (x : T) -> x)",
        &[forall(
            (0, 25),
            (2, 3),
            star(6, 7),
            lambda((12, 24), (13, 14), var(17, 18), var(23, 24)),
        )],
    )?;

    run_test_decl(
        "(x : * = P) (y : P = ((z : P) => z))",
        &[
            decl((0, 11), (1, 2), star(5, 6), var(9, 10)),
            decl(
                (12, 36),
                (13, 14),
                var(17, 18),
                forall((21, 35), (23, 24), var(27, 28), var(33, 34)),
            ),
        ],
    )?;

    Ok(())
}

#[derive(Clone)]
struct State<'a> {
    text: &'a str,
    context: Vec<(&'a str, Option<Expr>, Expr)>,
}

impl std::fmt::Debug for State<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.context)
    }
}

impl State<'_> {
    fn new(text: &str) -> State<'_> {
        let context = Vec::new();
        State { text, context }
    }

    fn push(&mut self, var: Span, val: Option<Expr>, typ: Expr) {
        self.context.push((var.string(self.text), val, typ));
    }

    fn pop(&mut self) {
        self.context.pop();
    }

    fn find_value(&self, var: Span) -> Result<Expr> {
        let var_ = var.string(self.text);
        for (key, val, _typ) in &self.context {
            if var_ == *key {
                return Ok(match val {
                    None => Expr::Var(var),
                    Some(val) => val.clone(),
                });
            }
        }
        err!(self.text, var, "could not find value for variable")
    }

    fn find_type(&self, var: Span) -> Result<Expr> {
        let var_ = var.string(self.text);
        for (key, _val, typ) in &self.context {
            if var_ == *key {
                return Ok(typ.clone());
            }
        }
        err!(self.text, var, "could not find type for variable")
    }
}

impl Expr {
    fn beta_reduce(&self, state: &mut State) -> Result<Expr> {
        Ok(match self {
            Expr::Type(span) => Expr::Type(*span),
            Expr::Var(var) => state.find_value(*var)?,
            Expr::App(span, f, x) => {
                let f = f.beta_reduce(state)?;
                let x = x.beta_reduce(state)?;
                match f {
                    Expr::Lambda(_, var, arg, ret) => {
                        state.push(var, Some(x), *arg);
                        let ret = ret.beta_reduce(state)?;
                        state.pop();
                        ret
                    }
                    _ => Expr::App(*span, Box::new(f), Box::new(x)),
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state)?;
                state.push(*var, None, arg.clone());
                let ret = ret.beta_reduce(state)?;
                state.pop();
                Expr::Lambda(*span, *var, Box::new(arg), Box::new(ret))
            }
            Expr::Forall(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state)?;
                state.push(*var, None, arg.clone());
                let ret = ret.beta_reduce(state)?;
                state.pop();
                Expr::Forall(*span, *var, Box::new(arg), Box::new(ret))
            }
        })
    }

    fn is_alpha_eq<'a>(
        state: &State<'a>,
        lhs: &Expr,
        rhs: &Expr,
        equal_vars: &mut Vec<(&'a str, &'a str)>,
    ) -> bool {
        match (lhs, rhs) {
            (Expr::Type(_), Expr::Type(_)) => true,

            (Expr::Var(lhs_var), Expr::Var(rhs_var)) => {
                let lhs_string = lhs_var.string(state.text);
                let rhs_string = rhs_var.string(state.text);
                equal_vars
                    .iter()
                    .find(|(lhs, rhs)| {
                        let lhs_matches = *lhs == lhs_string;
                        let rhs_matches = *rhs == rhs_string;
                        match (lhs_matches, rhs_matches) {
                            (true, true) => true,
                            (false, false) => false,
                            (true, false) | (false, true) => panic!("is_alpha_eq"),
                        }
                    })
                    .is_some()
            }

            (Expr::App(_, lhs_f, lhs_x), Expr::App(_, rhs_f, rhs_x)) => {
                Self::is_alpha_eq(state, lhs_f, rhs_f, equal_vars)
                    && Self::is_alpha_eq(state, lhs_x, rhs_x, equal_vars)
            }

            (
                Expr::Lambda(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Lambda(_, rhs_var, rhs_arg, rhs_ret),
            )
            | (
                Expr::Forall(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Forall(_, rhs_var, rhs_arg, rhs_ret),
            ) => {
                let args_eq = Self::is_alpha_eq(state, lhs_arg, rhs_arg, equal_vars);
                equal_vars.push((lhs_var.string(state.text), rhs_var.string(state.text)));
                let rets_eq = Self::is_alpha_eq(state, lhs_ret, rhs_ret, equal_vars);
                equal_vars.pop();
                args_eq && rets_eq
            }

            _ => false,
        }
    }

    fn typecheck(&self, state: &mut State) -> Result<Expr> {
        Ok(match self {
            Expr::Type(span) => return err!(state.text, span, "no type for *"),
            Expr::Var(var) => state.find_type(*var)?,
            Expr::App(span, f, x) => {
                let f_type = f.typecheck(state)?;
                let x_type = x.typecheck(state)?;
                match f_type {
                    Expr::Forall(span, var, arg, ret) => {
                        if Self::is_alpha_eq(state, &arg, &x_type, &mut Vec::new()) {
                            state.push(var, Some(*x.clone()), x_type);
                            let ret = ret.beta_reduce(state)?;
                            state.pop();
                            ret
                        } else {
                            let f = f.beta_reduce(state)?.to_string(state.text);
                            let x = x.beta_reduce(state)?.to_string(state.text);
                            let arg = arg.beta_reduce(state)?.to_string(state.text);
                            let x_type = x_type.beta_reduce(state)?.to_string(state.text);
                            return err!(
                                state.text,
                                span,
                                "wrong type for argument\nfunction: {f:?}\nargument: {x:?}\nexpected argument type: {arg:?}\nfound argument type: {x_type:?}"
                            );
                        }
                    }
                    _ => return err!(state.text, span, "expected forall type"),
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                state.push(*var, None, *arg.clone());
                let ret = ret.typecheck(state)?;
                state.pop();
                Expr::Forall(*span, *var, arg.clone(), Box::new(ret))
            }
            Expr::Forall(_, var, arg, ret) => {
                state.push(*var, None, *arg.clone());
                let ret = ret.typecheck(state)?;
                state.pop();
                ret
            }
        })
    }
}

#[test]
fn test_typecheck() -> Result<()> {
    fn run_test(text: &str, expected_eval: &str, expected_type: &str) -> Result<()> {
        let tokens = tokenize(text);
        let sexp = sexpify(&tokens, text)?.remove(0);
        let expr = parse_expr(text, &sexp)?;
        let actual_eval = expr.beta_reduce(&mut State::new(text))?.to_string(text);
        let actual_type = expr.typecheck(&mut State::new(text))?;
        assert_eq!(expected_eval, actual_eval);
        assert_eq!(expected_type, actual_type.to_string(text));
        Ok(())
    }

    assert_eq!(
        run_test("*", "", ""),
        Err(String::from("error at 1:1: no type for *\n*"))
    );
    run_test("((x : *) => x)", "((x : *) => x)", "*")?;
    run_test("((x : *) -> x)", "((x : *) -> x)", "((x : *) => *)")?;
    run_test("(((x : *) -> x) ((x : *) => x))", "((x : *) => x)", "*")?;

    run_test(
        "(((T : *) (t : T) -> t) ((S : *) => S))",
        "((t : ((S : *) => S)) -> t)",
        "((t : ((S : *) => S)) => ((S : *) => S))",
    )?;

    Ok(())
}

fn run(text: &str) -> Result<()> {
    println!();

    let tokens = tokenize(text);
    let sexps = sexpify(&tokens, text)?;
    let decls = sexps
        .into_iter()
        .map(|sexp| parse_decl(&sexp, text))
        .collect::<Result<Vec<_>>>()?;

    let mut state = State::new(text);
    for Decl(span, var, arg, val) in decls {
        let given = arg.beta_reduce(&mut state)?;
        let inferred = val.typecheck(&mut state)?;
        if Expr::is_alpha_eq(&state, &given, &inferred, &mut Vec::new()) {
            state.push(var, Some(*val), given);
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

    println!("SUCCESS");
    Ok(())
}

#[test]
fn test_run() -> Result<()> {
    run("
        (Unit : * = ((T : *) (_ : T) => T))
        (unit : Unit = ((T : *) (x : T) -> x))
    ")?;

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
