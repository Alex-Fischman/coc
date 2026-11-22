use crate::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Var {
    pub span: Span,
    pub index: usize,
    pub is_hole: bool,
}

impl Var {
    fn to_str(self, text: &str) -> &str {
        if self.is_hole {
            "_"
        } else {
            self.span.to_str(text)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
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

    #[must_use]
    pub fn to_string(&self, text: &str) -> String {
        match self {
            Expr::Star(_) => String::from("*"),
            Expr::Var(var) => String::from(var.to_str(text)),
            Expr::App(_, f, x) => format!("({} {})", f.to_string(text), x.to_string(text)),
            Expr::Lambda(_, var, arg, ret) => format!(
                "(({} : {}) -> {})",
                var.to_str(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
            Expr::Forall(_, var, arg, ret) => format!(
                "(({} : {}) => {})",
                var.to_str(text),
                arg.to_string(text),
                ret.to_string(text)
            ),
        }
    }
}

pub struct ParseState<'a> {
    text: &'a str,
    vars: Vec<(&'a str, usize)>,
    var_count: usize,
}

impl ParseState<'_> {
    #[must_use]
    pub fn new(text: &str) -> ParseState<'_> {
        ParseState {
            text,
            vars: Vec::new(),
            var_count: 0,
        }
    }

    fn var(&mut self, span: Span) -> Var {
        let index = self.var_count;
        self.var_count += 1;
        Var {
            span,
            index,
            is_hole: false,
        }
    }

    fn hole(&mut self, span: Span) -> Var {
        let index = self.var_count;
        self.var_count += 1;
        Var {
            span,
            index,
            is_hole: true,
        }
    }

    fn push(&mut self, var: Var) {
        self.vars.push((var.span.to_str(self.text), var.index));
    }

    fn pop(&mut self, var: Var) {
        let Some((_key, index)) = self.vars.pop() else {
            panic!("bug: nothing to pop")
        };
        assert_eq!(index, var.index, "bug: did not pop the right variable");
    }

    fn find(&self, span: Span) -> Result<Var> {
        for (key, index) in self.vars.iter().rev() {
            if *key == span.to_str(self.text) {
                return Ok(Var {
                    span,
                    index: *index,
                    is_hole: false,
                });
            }
        }
        err!(self.text, span, "unbound variable")
    }
}

fn parse_binding(span: Span, list: Vec<Sexp>, state: &mut ParseState) -> Result<(Span, Var, Expr)> {
    let Ok([Sexp::Atom(var), Sexp::Atom(colon), arg]) = TryInto::<[Sexp; 3]>::try_into(list) else {
        return err!(state.text, span, "expected binding");
    };

    if colon.to_str(state.text) != ":" {
        return err!(state.text, colon, "expected colon");
    }

    Ok((span, state.var(var), parse_expr(arg, state)?))
}

pub fn parse_expr(sexp: Sexp, state: &mut ParseState) -> Result<Expr> {
    Ok(match sexp {
        Sexp::Atom(span) => match span.to_str(state.text) {
            "*" => Expr::Star(span),
            _ => Expr::Var(state.find(span)?),
        },
        Sexp::List(span, list) => {
            // (binding+ arrow)* expr+
            let mut binding_sexps = Vec::new();
            let mut tail = Vec::new();
            for sexp in list {
                match sexp {
                    Sexp::Atom(span) if span.to_str(state.text) == "->" => {
                        for binding in tail {
                            binding_sexps.push((true, binding));
                        }
                        tail = Vec::new();
                    }
                    Sexp::Atom(span) if span.to_str(state.text) == "=>" => {
                        for binding in tail {
                            binding_sexps.push((false, binding));
                        }
                        tail = Vec::new();
                    }
                    _ => tail.push(sexp),
                }
            }

            let mut bindings = Vec::new();
            for (is_lambda, binding) in binding_sexps {
                let (span, var, arg) = match (is_lambda, binding) {
                    (true, Sexp::Atom(span)) => {
                        (span, state.var(span), Expr::Var(state.hole(span)))
                    }
                    (false, Sexp::Atom(span)) => {
                        (span, state.hole(span), parse_expr(Sexp::Atom(span), state)?)
                    }
                    (true, Sexp::List(span, list)) => parse_binding(span, list, state)?,
                    (false, Sexp::List(span, list)) => {
                        if list.len() == 3 && list[1].span().to_str(state.text) == ":" {
                            parse_binding(span, list, state)?
                        } else {
                            let arg = parse_expr(Sexp::List(span, list), state)?;
                            (span, state.hole(span), arg)
                        }
                    }
                };

                bindings.push((is_lambda, span, var, arg));
                state.push(var);
            }

            let mut expr = tail
                .into_iter()
                .map(|sexp| parse_expr(sexp, state))
                .reduce(|f, x| match (f, x) {
                    (Err(err), _) | (_, Err(err)) => Err(err),
                    (Ok(f), Ok(x)) => Ok(Expr::App(
                        f.span().through(x.span()),
                        Box::new(f),
                        Box::new(x),
                    )),
                })
                .unwrap_or_else(|| err!(state.text, span, "expected expression"))?;

            for (is_lambda, binding_span, var, arg) in bindings.into_iter().rev() {
                let span = binding_span.through(expr.span());

                expr = if is_lambda {
                    Expr::Lambda(span, var, Box::new(arg), Box::new(expr))
                } else {
                    Expr::Forall(span, var, Box::new(arg), Box::new(expr))
                };

                state.pop(var);
            }

            *expr.span_mut() = span;
            expr
        }
    })
}

#[derive(Clone, Debug, PartialEq)]
pub enum Action {
    Define(Span, Var, Expr, Expr),
    Assert(Span, Expr, Expr),
}

pub fn parse_actions(text: &str) -> Result<Vec<Action>> {
    let tokens = tokenize(text);
    let mut sexps = sexpify(&tokens, text)?;

    let mut state = ParseState::new(text);
    let mut actions = Vec::new();

    sexps.reverse();
    while let Some(action) = sexps.pop() {
        match action.span().to_str(text) {
            "define" => {
                let Some(Sexp::Atom(var)) = sexps.pop() else {
                    return err!(text, action.span(), "expected variable name");
                };
                let var = state.var(var);

                let Some(Sexp::Atom(colon)) = sexps.pop() else {
                    return err!(text, action.span(), "expected colon");
                };
                if colon.to_str(text) != ":" {
                    return err!(text, colon, "expected colon");
                }

                let Some(typ) = sexps.pop() else {
                    return err!(text, action.span(), "expected variable type");
                };
                let typ = parse_expr(typ, &mut state)?;

                let Some(Sexp::Atom(equals)) = sexps.pop() else {
                    return err!(text, action.span(), "expected equals");
                };
                if equals.to_str(text) != "=" {
                    return err!(text, equals, "expected equals");
                }

                let Some(val) = sexps.pop() else {
                    return err!(text, action.span(), "expected variable value");
                };
                let val = parse_expr(val, &mut state)?;

                let span = action.span().through(val.span());
                actions.push(Action::Define(span, var, typ, val));
                state.push(var);
            }
            "assert" => {
                let Some(x) = sexps.pop() else {
                    return err!(text, action.span(), "expected value");
                };
                let x = parse_expr(x, &mut state)?;

                let Some(y) = sexps.pop() else {
                    return err!(text, action.span(), "expected two values");
                };
                let y = parse_expr(y, &mut state)?;

                let span = action.span().through(y.span());
                actions.push(Action::Assert(span, x, y));
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
            is_hole: false,
        }
    }
    fn star(lo: usize, hi: usize) -> Expr {
        Expr::Star(Span { lo, hi })
    }
    fn evar(lo: usize, hi: usize, index: usize) -> Expr {
        Expr::Var(Var {
            span: Span { lo, hi },
            index,
            is_hole: false,
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
    fn define((lo, hi): (usize, usize), var: Var, typ: Expr, val: Expr) -> Action {
        Action::Define(Span { lo, hi }, var, typ, val)
    }
    fn assert((lo, hi): (usize, usize), x: Expr, y: Expr) -> Action {
        Action::Assert(Span { lo, hi }, x, y)
    }

    fn run_test_expr(text: &str, expected_expr: &Expr) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexp = sexpify(&tokens, text)?.remove(0);
        let actual_expr = parse_expr(actual_sexp, &mut ParseState::new(text))?;
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
            var(2, 3, 0),
            forall((6, 20), var(8, 9, 1), star(12, 13), evar(18, 19, 1)),
            evar(25, 26, 0),
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
        "define x : * = *
         define y : x = ((z : *) => z)
         assert x y",
        &[
            define((0, 16), var(7, 8, 0), star(11, 12), star(15, 16)),
            define(
                (26, 55),
                var(33, 34, 1),
                evar(37, 38, 0),
                forall((41, 55), var(43, 44, 2), star(47, 48), evar(53, 54, 2)),
            ),
            assert((65, 75), evar(72, 73, 0), evar(74, 75, 1)),
        ],
    )?;

    Ok(())
}
