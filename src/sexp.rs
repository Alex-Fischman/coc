use crate::*;

#[derive(Debug, PartialEq)]
pub enum Sexp {
    Atom(Span),
    List(Span, Vec<Sexp>),
}

impl Sexp {
    #[must_use]
    pub fn span(&self) -> Span {
        match self {
            Sexp::Atom(span) | Sexp::List(span, ..) => *span,
        }
    }
}

pub fn sexpify(tokens: &[Token], text: &str) -> Result<Vec<Sexp>> {
    let mut stack = Vec::new();
    let mut level = Vec::new();

    for Token(token_type, span) in tokens {
        match token_type {
            TokenType::OpenParen => {
                stack.push((level, span));
                level = Vec::new();
            }
            TokenType::CloseParen => {
                let Some((prev_level, left_span)) = stack.pop() else {
                    return err!(text, span, "extra close paren");
                };

                let list = Sexp::List(left_span.through(*span), level);
                level = prev_level;
                level.push(list);
            }
            TokenType::Identifier => level.push(Sexp::Atom(*span)),
        }
    }

    match stack.pop() {
        None => Ok(level),
        Some((_, span)) => err!(text, span, "unclosed open paren"),
    }
}

#[cfg(test)]
fn atom(lo: usize, hi: usize) -> Sexp {
    Sexp::Atom(Span { lo, hi })
}

#[cfg(test)]
fn list((lo, hi): (usize, usize), list: Vec<Sexp>) -> Sexp {
    Sexp::List(Span { lo, hi }, list)
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
    run_test("()", &[list((0, 2), vec![])])?;
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
            list((0, 3), vec![atom(1, 2)]),
            list(
                (3, 21),
                vec![
                    atom(4, 5),
                    list((5, 7), vec![]),
                    atom(7, 8),
                    list(
                        (8, 19),
                        vec![
                            atom(9, 10),
                            list(
                                (10, 17),
                                vec![
                                    atom(11, 12),
                                    list((12, 15), vec![atom(13, 14)]),
                                    atom(15, 16),
                                ],
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
