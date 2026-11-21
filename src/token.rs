use crate::*;

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
pub enum TokenType {
    OpenParen,
    CloseParen,
    Identifier,
}

#[derive(Debug, PartialEq)]
pub struct Token(pub TokenType, pub Span);

#[must_use]
pub fn tokenize(text: &str) -> Vec<Token> {
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
