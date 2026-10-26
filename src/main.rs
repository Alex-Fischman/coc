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
    Colon,
    Comment,
    Whitespace,
    Symbol,
}

fn char_type(c: char) -> CharType {
    match c {
        '(' => CharType::OpenParen,
        ')' => CharType::CloseParen,
        ':' => CharType::Colon,
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
            CharType::Colon => {
                cursor += c.len_utf8();
                TokenType::Identifier
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

#[derive(Clone, Copy, Debug, PartialEq)]
struct Var {
    span: Span,
    index: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Star(Span),
    Var(Var),
    App(Span, Box<Expr>, Box<Expr>),
    Lambda(Span, Var, Box<Expr>, Box<Expr>),
    Forall(Span, Var, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Star(span)
            | Expr::Var(Var { span, .. })
            | Expr::App(span, ..)
            | Expr::Lambda(span, ..)
            | Expr::Forall(span, ..) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Expr::Star(span)
            | Expr::Var(Var { span, .. })
            | Expr::App(span, ..)
            | Expr::Lambda(span, ..)
            | Expr::Forall(span, ..) => span,
        }
    }

    fn to_string(&self, text: &str) -> String {
        match self {
            Expr::Star(_) => String::from("*"),
            Expr::Var(var) => String::from(var.span.string(text)),
            Expr::App(_, f, x) => format!("({} {})", f.to_string(text), x.to_string(text)),
            Expr::Lambda(_, var, arg, ret) => format!(
                "(({} : {}) -> {})",
                var.span.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
            Expr::Forall(_, var, arg, ret) => format!(
                "(({} : {}) => {})",
                var.span.string(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
        }
    }
}

fn parse_expr<'a>(
    text: &'a str,
    sexp: &Sexp,
    vars: &mut Vec<(&'a str, usize)>,
    var_count: &mut usize,
) -> Result<Expr> {
    fn resolve_var<'a>(text: &'a str, span: Span, vars: &mut Vec<(&'a str, usize)>) -> Result<Var> {
        match vars.iter().find_map(|(key, index)| {
            if *key == span.string(text) {
                Some(*index)
            } else {
                None
            }
        }) {
            Some(index) => Ok(Var { span, index }),
            None => err!(text, span, "unbound variable"),
        }
    }

    fn parse_arrow_groups<'a>(
        text: &'a str,
        span: Span,
        vars: &mut Vec<(&'a str, usize)>,
        var_count: &mut usize,
        mut init_groups: Vec<(bool, Vec<&Sexp>)>,
        last_group: Vec<&Sexp>,
    ) -> Result<Expr> {
        let Some((is_lambda, first_group)) = init_groups.last_mut() else {
            return last_group
                .into_iter()
                .map(|sexp| parse_expr(text, sexp, vars, var_count))
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
                .unwrap_or_else(|| err!(text, span, "expected expression"));
        };

        let Some(binding) = first_group.pop() else {
            init_groups.pop();
            return parse_arrow_groups(text, span, vars, var_count, init_groups, last_group);
        };

        let is_lambda = *is_lambda;
        let binding_span = binding.span();

        let (var_span, arg) = match binding {
            Sexp::Atom(span) => return err!(text, span, "expected binding"),
            Sexp::List(span, list) => match &list[..] {
                [Sexp::Atom(var), Sexp::Atom(colon), arg] => {
                    if colon.string(text) != ":" {
                        return err!(text, span, "expected colon");
                    }
                    (*var, Box::new(parse_expr(text, arg, vars, var_count)?))
                }
                _ => return err!(text, span, "expected binding"),
            },
        };

        let var_index = *var_count;
        let var = Var {
            span: var_span,
            index: var_index,
        };
        *var_count += 1;
        vars.push((var_span.string(text), var_index));
        let ret = parse_arrow_groups(text, span, vars, var_count, init_groups, last_group)?;
        vars.pop();

        let span = Span {
            lo: binding_span.lo,
            hi: ret.span().hi,
        };

        Ok(if is_lambda {
            Expr::Lambda(span, var, arg, Box::new(ret))
        } else {
            Expr::Forall(span, var, arg, Box::new(ret))
        })
    }

    Ok(match sexp {
        Sexp::Atom(span) => match span.string(text) {
            "*" => Expr::Star(*span),
            _ => Expr::Var(resolve_var(text, *span, vars)?),
        },
        Sexp::List(span, list) => {
            let mut init_groups = Vec::new();
            let mut last_group = Vec::new();
            for sexp in list {
                match sexp {
                    Sexp::Atom(span) if span.string(text) == "->" => {
                        last_group.reverse();
                        init_groups.push((true, last_group));
                        last_group = Vec::new();
                    }
                    Sexp::Atom(span) if span.string(text) == "=>" => {
                        last_group.reverse();
                        init_groups.push((false, last_group));
                        last_group = Vec::new();
                    }
                    _ => last_group.push(sexp),
                }
            }
            init_groups.reverse();
            let mut expr =
                parse_arrow_groups(text, *span, vars, var_count, init_groups, last_group)?;
            *expr.span_mut() = *span;
            expr
        }
    })
}

#[derive(Clone, Debug, PartialEq)]
enum Action {
    Define(Span, Var, Box<Expr>),
    Assert(Span, Box<Expr>, Box<Expr>),
}

fn parse_actions(text: &str) -> Result<Vec<Action>> {
    let tokens = tokenize(text);
    let mut sexps = sexpify(&tokens, text)?;

    let mut vars = Vec::new();
    let mut var_count = 0;
    let mut actions = Vec::new();

    sexps.reverse();
    while let Some(action) = sexps.pop() {
        match action.span().string(text) {
            "define" => {
                let Some(Sexp::Atom(var_span)) = sexps.pop() else {
                    return err!(text, action.span(), "expected variable name");
                };
                if sexps.pop().is_some_and(|x| x.span().string(text) != ":") {
                    return err!(text, action.span(), "expected colon");
                }
                let Some(val) = sexps
                    .pop()
                    .map(|x| parse_expr(text, &x, &mut vars, &mut var_count))
                    .transpose()?
                else {
                    return err!(text, action.span(), "expected variable value");
                };
                let span = Span {
                    lo: action.span().lo,
                    hi: val.span().hi,
                };

                let var_index = var_count;
                let var = Var {
                    span: var_span,
                    index: var_index,
                };
                var_count += 1;
                vars.push((var_span.string(text), var_index));

                actions.push(Action::Define(span, var, Box::new(val)));
            }
            "assert" => {
                let Some(x) = sexps
                    .pop()
                    .map(|x| parse_expr(text, &x, &mut vars, &mut var_count))
                    .transpose()?
                else {
                    return err!(text, action.span(), "expected value");
                };
                let Some(y) = sexps
                    .pop()
                    .map(|x| parse_expr(text, &x, &mut vars, &mut var_count))
                    .transpose()?
                else {
                    return err!(text, action.span(), "expected two values");
                };
                let span = Span {
                    lo: action.span().lo,
                    hi: y.span().hi,
                };
                actions.push(Action::Assert(span, Box::new(x), Box::new(y)));
            }
            _ => return err!(text, action.span(), "expected define or assert"),
        }
    }
    Ok(actions)
}

#[test]
fn test_parse() -> Result<()> {
    fn var(lo: usize, hi: usize, index: usize) -> Var {
        Var {
            span: Span { lo, hi },
            index,
        }
    }
    fn star(lo: usize, hi: usize) -> Expr {
        Expr::Star(Span { lo, hi })
    }
    fn evar(lo: usize, hi: usize, index: usize) -> Expr {
        Expr::Var(Var {
            span: Span { lo, hi },
            index,
        })
    }
    fn app((lo, hi): (usize, usize), f: Expr, x: Expr) -> Expr {
        Expr::App(Span { lo, hi }, Box::new(f), Box::new(x))
    }
    fn lambda((lo, hi): (usize, usize), var: Var, arg: Expr, ret: Expr) -> Expr {
        Expr::Lambda(Span { lo, hi }, var, Box::new(arg), Box::new(ret))
    }
    fn forall((lo, hi): (usize, usize), var: Var, arg: Expr, ret: Expr) -> Expr {
        Expr::Forall(Span { lo, hi }, var, Box::new(arg), Box::new(ret))
    }
    fn define((lo, hi): (usize, usize), var: Var, val: Expr) -> Action {
        Action::Define(Span { lo, hi }, var, Box::new(val))
    }
    fn assert((lo, hi): (usize, usize), x: Expr, y: Expr) -> Action {
        Action::Assert(Span { lo, hi }, Box::new(x), Box::new(y))
    }

    fn run_test_expr(text: &str, expected_expr: &Expr) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexp = sexpify(&tokens, text)?.remove(0);
        let actual_expr = parse_expr(text, &actual_sexp, &mut Vec::new(), &mut 0)?;
        assert_eq!(expected_expr, &actual_expr);
        Ok(())
    }

    fn run_test_actions(text: &str, expected_actions: &[Action]) -> Result<()> {
        let actual_actions = parse_actions(text)?;
        assert_eq!(expected_actions, actual_actions);
        Ok(())
    }

    run_test_expr("*", &star(0, 1))?;
    {
        let f_x = app((23, 26), evar(23, 24, 0), evar(25, 26, 1));
        let f_x_y = app((22, 29), f_x, evar(27, 28, 2));
        let lambda_y = lambda((13, 29), var(14, 15, 2), star(16, 17), f_x_y);
        let lambda_x = lambda((7, 29), var(8, 9, 1), star(10, 11), lambda_y);
        let lambda_f = lambda((0, 30), var(2, 3, 0), star(4, 5), lambda_x);
        run_test_expr("((f:*) (x:*) (y:*) -> (f x y))", &lambda_f)?;
    }
    run_test_expr(
        "((x:*) -> x)",
        &lambda((0, 12), var(2, 3, 0), star(4, 5), evar(10, 11, 0)),
    )?;
    run_test_expr(
        "((_:*) => *)",
        &forall((0, 12), var(2, 3, 0), star(4, 5), star(10, 11)),
    )?;
    run_test_expr(
        "((x : ((A : *) => A)) -> x)",
        &lambda(
            (0, 27),
            var(2, 3, 1),
            forall((6, 20), var(8, 9, 0), star(12, 13), evar(18, 19, 0)),
            evar(25, 26, 1),
        ),
    )?;
    run_test_expr(
        "((T : *) => (x : T) -> x)",
        &forall(
            (0, 25),
            var(2, 3, 0),
            star(6, 7),
            lambda((12, 24), var(13, 14, 1), evar(17, 18, 0), evar(23, 24, 1)),
        ),
    )?;

    run_test_actions(
        "define x: *
         define y: ((z : *) => z)
         assert x y",
        &[
            define((0, 11), var(7, 8, 0), star(10, 11)),
            define(
                (21, 45),
                var(28, 29, 2),
                forall((31, 45), var(33, 34, 1), star(37, 38), evar(43, 44, 1)),
            ),
            assert((55, 65), evar(62, 63, 0), evar(64, 65, 2)),
        ],
    )?;

    Ok(())
}

#[derive(Clone)]
struct State<'a> {
    text: &'a str,
    context: Vec<(usize, Option<Expr>, Expr)>,
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

    fn push(&mut self, var: Var, val: Option<Expr>, typ: Expr) {
        self.context.push((var.index, val, typ));
    }

    fn pop(&mut self, var: Var) {
        let Some((key, _val, _typ)) = self.context.pop() else {
            panic!("bug: nothing to pop")
        };
        assert_eq!(key, var.index, "bug: did not pop the right variable");
    }

    fn find_value(&self, var: Var) -> Result<Expr> {
        for (key, val, _typ) in &self.context {
            if var.index == *key {
                return Ok(match val {
                    None => Expr::Var(var),
                    Some(val) => val.clone(),
                });
            }
        }
        err!(self.text, var.span, "could not find value for variable")
    }

    fn find_type(&self, var: Var) -> Result<Expr> {
        for (key, _val, typ) in &self.context {
            if var.index == *key {
                return Ok(typ.clone());
            }
        }
        err!(self.text, var.span, "could not find type for variable")
    }
}

impl Expr {
    fn beta_reduce(&self, state: &mut State) -> Result<Expr> {
        Ok(match self {
            Expr::Star(span) => Expr::Star(*span),
            Expr::Var(var) => state.find_value(*var)?,
            Expr::App(span, f, x) => {
                let f = f.beta_reduce(state)?;
                let x = x.beta_reduce(state)?;
                match f {
                    Expr::Lambda(_, var, arg, ret) => {
                        state.push(var, Some(x), *arg);
                        let ret = ret.beta_reduce(state)?;
                        state.pop(var);
                        ret
                    }
                    _ => Expr::App(*span, Box::new(f), Box::new(x)),
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state)?;
                state.push(*var, None, arg.clone());
                let ret = ret.beta_reduce(state)?;
                state.pop(*var);
                Expr::Lambda(*span, *var, Box::new(arg), Box::new(ret))
            }
            Expr::Forall(span, var, arg, ret) => {
                let arg = arg.beta_reduce(state)?;
                state.push(*var, None, arg.clone());
                let ret = ret.beta_reduce(state)?;
                state.pop(*var);
                Expr::Forall(*span, *var, Box::new(arg), Box::new(ret))
            }
        })
    }

    fn is_alpha_eq(lhs: &Expr, rhs: &Expr, equal_vars: &mut Vec<(usize, usize)>) -> bool {
        match (lhs, rhs) {
            (Expr::Star(_), Expr::Star(_)) => true,

            (Expr::Var(lhs_var), Expr::Var(rhs_var)) => equal_vars
                .iter()
                .find_map(|(lhs, rhs)| {
                    let lhs_matches = *lhs == lhs_var.index;
                    let rhs_matches = *rhs == rhs_var.index;
                    match (lhs_matches, rhs_matches) {
                        (false, false) => None,
                        (true, true) => Some(true),
                        (true, false) | (false, true) => Some(false),
                    }
                })
                .expect("bug: unbound variable"),

            (Expr::App(_, lhs_f, lhs_x), Expr::App(_, rhs_f, rhs_x)) => {
                let f_eq = Self::is_alpha_eq(lhs_f, rhs_f, equal_vars);
                let x_eq = Self::is_alpha_eq(lhs_x, rhs_x, equal_vars);
                f_eq && x_eq
            }

            (
                Expr::Lambda(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Lambda(_, rhs_var, rhs_arg, rhs_ret),
            )
            | (
                Expr::Forall(_, lhs_var, lhs_arg, lhs_ret),
                Expr::Forall(_, rhs_var, rhs_arg, rhs_ret),
            ) => {
                let args_eq = Self::is_alpha_eq(lhs_arg, rhs_arg, equal_vars);
                equal_vars.push((lhs_var.index, rhs_var.index));
                let rets_eq = Self::is_alpha_eq(lhs_ret, rhs_ret, equal_vars);
                equal_vars.pop();
                args_eq && rets_eq
            }

            _ => false,
        }
    }

    fn typecheck(&self, state: &mut State) -> Result<Expr> {
        Ok(match self {
            Expr::Star(span) => return err!(state.text, span, "no type for *"),
            Expr::Var(var) => state.find_type(*var)?,
            Expr::App(span, f, x) => {
                let f_type = f.typecheck(state)?.beta_reduce(state)?;
                let x_type = x.typecheck(state)?.beta_reduce(state)?;
                if let Expr::Forall(_, var, arg, ret) = f_type {
                    if Self::is_alpha_eq(&arg, &x_type, &mut Vec::new()) {
                        state.push(var, Some(*x.clone()), x_type);
                        let ret = ret.beta_reduce(state)?;
                        state.pop(var);
                        ret
                    } else {
                        let f = f.to_string(state.text);
                        let x = x.to_string(state.text);
                        let arg = arg.to_string(state.text);
                        let x_type = x_type.to_string(state.text);
                        return err!(
                            state.text,
                            span,
                            "wrong type for argument\nfunction: {f}\nargument: {x}\nexpected argument type: {arg}\nfound argument type: {x_type}"
                        );
                    }
                } else {
                    let f = f.to_string(state.text);
                    let f_type = f_type.to_string(state.text);
                    return err!(
                        state.text,
                        span,
                        "expected value to have a forall type\nvalue: {f}\ntype: {f_type}"
                    );
                }
            }
            Expr::Lambda(span, var, arg, ret) => {
                state.push(*var, None, *arg.clone());
                let ret = ret.typecheck(state)?;
                state.pop(*var);
                Expr::Forall(*span, *var, arg.clone(), Box::new(ret))
            }
            Expr::Forall(_, var, arg, ret) => {
                state.push(*var, None, *arg.clone());
                let ret = ret.typecheck(state)?;
                state.pop(*var);
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
        let expr = parse_expr(text, &sexp, &mut Vec::new(), &mut 0)?;

        let mut state = State::new(text);
        let actual_eval = expr.beta_reduce(&mut state)?.to_string(text);
        let actual_type = expr.typecheck(&mut state)?;
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

    let mut state = State::new(text);
    for action in parse_actions(text)? {
        match action {
            Action::Define(_span, var, val) => {
                let typ = val.typecheck(&mut state)?.beta_reduce(&mut state)?;
                state.push(var, Some(*val), typ);
            }
            Action::Assert(span, x, y) => {
                let x_type = x.typecheck(&mut state)?.beta_reduce(&mut state)?;
                let y_type = y.typecheck(&mut state)?.beta_reduce(&mut state)?;
                if !Expr::is_alpha_eq(&x_type, &y_type, &mut Vec::new()) {
                    let x = x.to_string(text);
                    let y = y.to_string(text);
                    let x_type = x_type.to_string(text);
                    let y_type = y_type.to_string(text);
                    return err!(
                        state.text,
                        span,
                        "assert arguments had different types\n{x}\n: {x_type}\n{y}\n: {y_type}"
                    );
                }
                let x = x.beta_reduce(&mut state)?;
                let y = y.beta_reduce(&mut state)?;
                if !Expr::is_alpha_eq(&x, &y, &mut Vec::new()) {
                    let x = x.to_string(text);
                    let y = y.to_string(text);
                    return err!(state.text, span, "assertion failed\n{x}\n{y}");
                }
            }
        }
    }

    println!("SUCCESS");
    Ok(())
}

#[test]
fn test_run() -> Result<()> {
    run("
        define Unit: ((T : *) (_ : T) => T)
        define unit: ((T : *) (x : T) -> x)
        assert unit unit
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
