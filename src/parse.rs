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

struct State<'a> {
    text: &'a str,
    vars: Vec<(&'a str, usize)>,
    var_count: usize,
}

impl State<'_> {
    fn new(text: &str) -> State<'_> {
        State {
            text,
            vars: Vec::new(),
            var_count: 0,
        }
    }

    fn var(&mut self, span: Span, is_hole: bool) -> Var {
        let index = self.var_count;
        self.var_count += 1;
        Var {
            span,
            index,
            is_hole,
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

fn parse_expr(sexp: &Sexp, state: &mut State) -> Result<Expr> {
    fn parse_arrow_groups(
        span: Span,
        state: &mut State,
        mut init_groups: Vec<(bool, Vec<&Sexp>)>,
        last_group: Vec<&Sexp>,
    ) -> Result<Expr> {
        let Some((is_lambda, first_group)) = init_groups.last_mut() else {
            return last_group
                .into_iter()
                .map(|sexp| parse_expr(sexp, state))
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
                .unwrap_or_else(|| err!(state.text, span, "expected expression"));
        };

        let Some(binding) = first_group.pop() else {
            init_groups.pop();
            return parse_arrow_groups(span, state, init_groups, last_group);
        };

        let is_lambda = *is_lambda;
        let binding_span = binding.span();

        let (var, arg, ret) = match binding {
            Sexp::Atom(atom_span) => {
                if is_lambda {
                    let var = state.var(*atom_span, false);
                    let arg = Expr::Var(state.var(*atom_span, true));

                    state.push(var);
                    let ret = parse_arrow_groups(span, state, init_groups, last_group)?;
                    state.pop(var);

                    (var, arg, ret)
                } else {
                    let var = state.var(*atom_span, true);
                    let arg = Expr::Var(state.var(*atom_span, false));
                    let ret = parse_arrow_groups(span, state, init_groups, last_group)?;
                    (var, arg, ret)
                }
            }
            Sexp::List(list_span, list) => match &list[..] {
                [Sexp::Atom(var_span), Sexp::Atom(colon), arg] => {
                    if colon.to_str(state.text) != ":" {
                        return err!(state.text, list_span, "expected colon");
                    }

                    let var = state.var(*var_span, false);
                    let arg = parse_expr(arg, state)?;

                    state.push(var);
                    let ret = parse_arrow_groups(span, state, init_groups, last_group)?;
                    state.pop(var);

                    (var, arg, ret)
                }
                _ => return err!(state.text, span, "expected binding"),
            },
        };

        let span = Span {
            lo: binding_span.lo,
            hi: ret.span().hi,
        };

        Ok(if is_lambda {
            Expr::Lambda(span, var, Box::new(arg), Box::new(ret))
        } else {
            Expr::Forall(span, var, Box::new(arg), Box::new(ret))
        })
    }

    Ok(match sexp {
        Sexp::Atom(span) => match span.to_str(state.text) {
            "*" => Expr::Star(*span),
            _ => Expr::Var(state.find(*span)?),
        },
        Sexp::List(span, list) => {
            let mut init_groups = Vec::new();
            let mut last_group = Vec::new();
            for sexp in list {
                match sexp {
                    Sexp::Atom(span) if span.to_str(state.text) == "->" => {
                        last_group.reverse();
                        init_groups.push((true, last_group));
                        last_group = Vec::new();
                    }
                    Sexp::Atom(span) if span.to_str(state.text) == "=>" => {
                        last_group.reverse();
                        init_groups.push((false, last_group));
                        last_group = Vec::new();
                    }
                    _ => last_group.push(sexp),
                }
            }
            init_groups.reverse();
            let mut expr = parse_arrow_groups(*span, state, init_groups, last_group)?;
            *expr.span_mut() = *span;
            expr
        }
    })
}

#[derive(Clone, Debug, PartialEq)]
pub enum Action {
    Define(Span, Var, Box<Expr>),
    Assert(Span, Box<Expr>, Box<Expr>),
}

pub fn parse_actions(text: &str) -> Result<Vec<Action>> {
    let tokens = tokenize(text);
    let mut sexps = sexpify(&tokens, text)?;

    let mut state = State::new(text);
    let mut actions = Vec::new();

    sexps.reverse();
    while let Some(action) = sexps.pop() {
        match action.span().to_str(text) {
            "define" => {
                let Some(Sexp::Atom(var_span)) = sexps.pop() else {
                    return err!(text, action.span(), "expected variable name");
                };
                if sexps.pop().is_some_and(|x| x.span().to_str(text) != ":") {
                    return err!(text, action.span(), "expected colon");
                }
                let Some(val) = sexps
                    .pop()
                    .map(|x| parse_expr(&x, &mut state))
                    .transpose()?
                else {
                    return err!(text, action.span(), "expected variable value");
                };
                let span = Span {
                    lo: action.span().lo,
                    hi: val.span().hi,
                };

                let var = state.var(var_span, false);
                state.push(var);
                actions.push(Action::Define(span, var, Box::new(val)));
            }
            "assert" => {
                let Some(x) = sexps
                    .pop()
                    .map(|x| parse_expr(&x, &mut state))
                    .transpose()?
                else {
                    return err!(text, action.span(), "expected value");
                };
                let Some(y) = sexps
                    .pop()
                    .map(|x| parse_expr(&x, &mut state))
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
    fn define((lo, hi): (usize, usize), var: Var, val: Expr) -> Action {
        Action::Define(Span { lo, hi }, var, Box::new(val))
    }
    fn assert((lo, hi): (usize, usize), x: Expr, y: Expr) -> Action {
        Action::Assert(Span { lo, hi }, Box::new(x), Box::new(y))
    }

    fn run_test_expr(text: &str, expected_expr: &Expr) -> Result<()> {
        let tokens = tokenize(text);
        let actual_sexp = sexpify(&tokens, text)?.remove(0);
        let actual_expr = parse_expr(&actual_sexp, &mut State::new(text))?;
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
