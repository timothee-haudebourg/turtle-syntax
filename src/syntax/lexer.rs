use std::iter::Peekable;
use std::io;
use std::fmt;
use source_span::{Position, Span};
use crate::Loc;

pub enum Error {
    IO(std::io::Error),
    WrongCloser(Delimiter, char),
    MissingCloser(Delimiter),
    IncompleteString,
    Unexpected(char),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match self {
            IO(e) => write!(f, "I/O: {}", e),
            WrongCloser(_, _) => write!(f, "wrong delimiter"),
            MissingCloser(_) => write!(f, "missing delimiter"),
            IncompleteString => write!(f, "incomplete string"),
            Unexpected(c) => write!(f, "unexpected character `{}`", c)
        }
    }
}

pub type Result<T> = std::result::Result<T, Loc<Error>>;

#[derive(Clone, Debug)]
pub enum Keyword {
    Prefix,
    Base,
    IriTag,
    LangTag(String),
    True,
    False
}

use super::ast::Numeric;

#[derive(Clone, Debug)]
pub enum Token {
    Punct(char),
    Keyword(Keyword),
    Ident(String),
    Group(Delimiter, Vec<Loc<Token>>),
    String(String),
    Iri(String),
    Numeric(Numeric)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Delimiter {
    Parenthesis,
    Bracket
}

impl Delimiter {
    pub fn closer(&self) -> char {
        use self::Delimiter::*;
        match self {
            Parenthesis => ')',
            Bracket => ']'
        }
    }
}

pub struct Lexer<I: Iterator<Item = io::Result<char>>> {
    input: Peekable<I>,
    pos: Position
}

impl<I: Iterator<Item = io::Result<char>>> Lexer<I> {
    pub fn new(input: I) -> Lexer<I> {
        Lexer {
            input: input.peekable(),
            pos: Position::default()
        }
    }
}

fn peek<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &Span) -> Result<Option<char>> {
    match it.peek() {
        Some(Ok(c)) => {
            Ok(Some(*c))
        },
        Some(Err(_)) => {
            let mut dummy_span = *span;
            Ok(consume(it, &mut dummy_span)?) // always return an error.
        },
        None => Ok(None)
    }
}

fn consume<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &mut Span) -> Result<Option<char>> {
    match it.next() {
        Some(Ok(c)) => {
            span.push(c);
            Ok(Some(c))
        },
        Some(Err(e)) => Err(Loc::new(Error::IO(e), span.end().into())),
        None => Ok(None)
    }
}

fn parse_group<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Loc<Token>> {
    let mut span = pos.into();

    let delimiter = match consume(it, &mut span)?.unwrap() {
        '(' => Delimiter::Parenthesis,
        '[' => Delimiter::Bracket,
        _ => unreachable!()
    };

    let mut tokens = Vec::new();
    skip_whitespaces(it, &mut span)?;

    loop {
        match peek(it, &span)? {
            Some(')') | Some(']') => {
                let c = consume(it, &mut span)?.unwrap();
                if c == delimiter.closer() {
                    return Ok(Loc::new(Token::Group(delimiter, tokens), span))
                } else {
                    return Err(Loc::new(Error::WrongCloser(delimiter, c), span))
                }
            },
            Some(_) => {
                let token = parse_token(it, span.end())?.unwrap();
                span.append(token.span());
                tokens.push(token);

                skip_whitespaces(it, &mut span)?;
            },
            None => return Err(Loc::new(Error::MissingCloser(delimiter), span))
        }
    }
}

fn parse_string<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Loc<Token>> {
    let mut span = pos.into();
    consume(it, &mut span)?;

    let mut escape = false;
    let mut string = String::new();

    loop {
        if let Some(c) = consume(it, &mut span)? {
            match c {
                '"' => {
                    return Ok(Loc::new(Token::String(string), span))
                },
                '\\' => {
                    if let Some(c) = consume(it, &mut span)? {
                        match c {
                            'r' => string.push('\r'),
                            'n' => string.push('\n'),
                            't' => string.push('\t'),
                            _ => string.push(c)
                        }
                    } else {
                        return Err(Loc::new(Error::IncompleteString, span))
                    }
                },
                _ => string.push(c)
            }
        } else {
            return Err(Loc::new(Error::IncompleteString, span))
        }
    }
}

fn parse_iri<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Loc<Token>> {
    let mut span = pos.into();
    consume(it, &mut span)?;

    let mut string = String::new();

    loop {
        if let Some(c) = consume(it, &mut span)? {
            match c {
                '>' => {
                    break
                },
                _ => string.push(c)
            }
        } else {
            return Err(Loc::new(Error::IncompleteString, span))
        }
    }

    Ok(Loc::new(Token::Iri(string), span))
}

fn is_space(c: char) -> bool {
    c.is_whitespace() || c.is_control() || c == '\n'
}

fn is_punct(c: char) -> bool {
    c == '.' || c == ',' || c == ';'
}

fn is_separator(c: char) -> bool {
    is_space(c) || is_punct(c) || c == '<' || c == '(' || c == ')' || c == '[' || c == ']' || c == '"' || c == '@'
}

fn parse_ident<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, mut span: Span) -> Result<Loc<Token>> {
    let mut id = String::new();

    loop {
        if let Some(c) = peek(it, &span)? {
            if !id.is_empty() && is_separator(c) {
                break
            } else {
                consume(it, &mut span)?;
                id.push(c);

                if id == "^^" {
                    break
                }
            }
        } else {
            break
        }
    }

    let token = match id.as_str() {
        "@prefix" => Token::Keyword(Keyword::Prefix),
        "@base" => Token::Keyword(Keyword::Base),
        lang if lang.chars().next() == Some('@') => Token::Keyword(Keyword::LangTag(lang.to_string())),
        "^^" => Token::Keyword(Keyword::IriTag),
        "true" => Token::Keyword(Keyword::True),
        "false" => Token::Keyword(Keyword::False),
        _ => Token::Ident(id)
    };

    Ok(Loc::new(token, span))
}

fn parse_numeric<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, sign: bool, mut span: Span) -> Result<Loc<Token>> {
    let mut integer = 0;
    let mut decimal = 0;
    let mut exponent_sign = true;
    let mut exponent = 0;

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum State {
        Integer,
        Decimal,
        ExponentSign,
        ExponentDigit,
        Exponent
    };

    let mut state = State::Integer;

    loop {
        if let Some(c) = peek(it, &span)? {
            match state {
                State::Integer if c.is_digit(10) => {
                    consume(it, &mut span)?;
                    integer = integer * 10 + c.to_digit(10).unwrap() as i64;
                },
                State::Integer if c == '.' => {
                    consume(it, &mut span)?;
                    state = State::Decimal;
                }
                State::Decimal if c.is_digit(10) => {
                    consume(it, &mut span)?;
                    decimal = decimal * 10 + c.to_digit(10).unwrap() as u32;
                },
                State::Decimal if c == 'e' || c == 'E' => {
                    consume(it, &mut span)?;
                    state = State::ExponentSign;
                },
                State::ExponentSign if c == '+' => {
                    consume(it, &mut span)?;
                    state = State::ExponentDigit;
                },
                State::ExponentSign if c == '-' => {
                    consume(it, &mut span)?;
                    exponent_sign = false;
                    state = State::ExponentDigit;
                },
                State::ExponentSign | State::Exponent | State::ExponentDigit if c.is_digit(10) => {
                    consume(it, &mut span)?;
                    exponent = exponent * 10 + c.to_digit(10).unwrap() as i32;
                    state = State::Exponent;
                },
                State::ExponentSign | State::ExponentDigit => {
                    return Err(Loc::new(Error::Unexpected(c), span))
                },
                _ => break
            }
        } else {
            break
        }
    }

    let n = match state {
        State::Integer if sign => Numeric::Int(integer),
        State::Integer if !sign => Numeric::Int(-integer),
        State::Decimal => Numeric::Decimal(sign, integer as u32, decimal as u32),
        State::Exponent if exponent_sign => Numeric::Double(sign, integer as u32, decimal as u32, exponent),
        State::Exponent if !exponent_sign => Numeric::Double(sign, integer as u32, decimal as u32, -exponent),
        _ => return Err(Loc::new(Error::IO(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end of stream")), span))
    };

    Ok(Loc::new(Token::Numeric(n), span))
}

fn skip_whitespaces<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, span: &mut Span) -> Result<()> {
    loop {
        match peek(it, span)? {
            Some('#') => {
                loop {
                    match consume(it, span)? {
                        None | Some('\n') => break,
                        _ => ()
                    }
                }
            },
            Some(c) if is_space(c) => {
                consume(it, span)?;
            }
            _ => break
        }
    }

    Ok(())
}

fn parse_token<I: Iterator<Item = io::Result<char>>>(it: &mut Peekable<I>, pos: Position) -> Result<Option<Loc<Token>>> {
    let mut whitespace_span = pos.into();
    skip_whitespaces(it, &mut whitespace_span)?;
    match peek(it, &whitespace_span)? {
        Some(c) if is_punct(c) => {
            let mut span = whitespace_span.end().into();
            consume(it, &mut span)?;
            Ok(Some(Loc::new(Token::Punct(c), span)))
        }
        Some('(') | Some('[') => Ok(Some(parse_group(it, whitespace_span.end())?)),
        Some(')') | Some(']') => {
            let mut span = whitespace_span.end().into();
            let c = consume(it, &mut span)?.unwrap();
            Err(Loc::new(Error::Unexpected(c), span))
        },
        Some('"') => Ok(Some(parse_string(it, whitespace_span.end())?)),
        Some('<') => Ok(Some(parse_iri(it, whitespace_span.end())?)),
        Some('-') => {
            let mut span = whitespace_span.end().into();
            consume(it, &mut span)?;
            Ok(Some(parse_numeric(it, false, span)?))
        },
        Some('+') => {
            let mut span = whitespace_span.end().into();
            consume(it, &mut span)?;
            Ok(Some(parse_numeric(it, true, span)?))
        },
        Some(c) if c.is_digit(10) => {
            let span = whitespace_span.end().into();
            Ok(Some(parse_numeric(it, true, span)?))
        },
        Some(_) => {
            let span = whitespace_span.end().into();
            Ok(Some(parse_ident(it, span)?))
        },
        None => Ok(None)
    }
}

impl<I: Iterator<Item = io::Result<char>>> Iterator for Lexer<I> {
    type Item = Result<Loc<Token>>;

    fn next(&mut self) -> Option<Result<Loc<Token>>> {
        match parse_token(&mut self.input, self.pos) {
            Ok(Some(token)) => {
                self.pos = token.span().end();
                Some(Ok(token))
            },
            Ok(None) => None,
            Err(e) => {
                self.pos = e.span().end();
                Some(Err(e))
            }
        }
    }
}
