//! Turtle lexer.

use std::iter::Peekable;
use std::fmt;
use std::marker::PhantomData;
use iref::IriBuf;
use source_span::{Position, Span, Metrics, Loc};

/// Lexing error.
#[derive(Debug)]
pub enum Error<E: std::error::Error> {
	/// IO error.
	IO(E),

	/// Unexpected end of char stream.
	UnexpectedEos,

	/// Wrong closing delimiter.
	WrongCloser(Delimiter, char),

	/// Missing closing delimiter.
	MissingCloser(Delimiter),

	/// Unclosed string.
	UnclosedString,

	/// Unclosed IRI.
	UnclosedIri,

	/// Unexpected character.
	Unexpected(char),

	/// Invalid IRI.
	InvalidIri(String)
}

impl<E: std::error::Error> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Error::*;
		match self {
			IO(e) => write!(f, "I/O: {}", e),
			UnexpectedEos => write!(f, "unexpected end of stream"),
			WrongCloser(_, _) => write!(f, "wrong delimiter"),
			MissingCloser(_) => write!(f, "missing delimiter"),
			UnclosedString => write!(f, "unclosed string"),
			UnclosedIri => write!(f, "unclosed IRI"),
			Unexpected(c) => write!(f, "unexpected character `{}`", c),
			InvalidIri(str) => write!(f, "invalid iri `{}`", str)
		}
	}
}

/// Lexing result.
pub type Result<T, E> = std::result::Result<T, Loc<Error<E>>>;

/// Turtle keywords.
#[derive(Clone, Debug)]
pub enum Keyword {
	/// `@prefix`
	Prefix,

	/// `@base`
	Base,

	/// IRI tag.
	IriTag,

	/// Lang tag.
	LangTag(String),

	/// `true`
	True,

	/// `false`
	False
}

/// Numerical value.
use super::ast::Numeric;

/// Token.
#[derive(Clone, Debug)]
pub enum Token {
	/// Punctuation (`,`, `.`, `;`, etc.).
	Punct(char),

	/// Keyword.
	Keyword(Keyword),

	/// Identifier.
	Ident(String),

	/// Group.
	Group(Delimiter, Vec<Loc<Token>>),

	/// String.
	String(String),

	/// IRI.
	Iri(IriBuf),

	/// NUmerical value.
	Numeric(Numeric)
}

/// Group delimiter.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum Delimiter {
	/// Parenthesis.
	Parenthesis,

	/// Bracket.
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

/// Lexer. Transforms a `char` iterator into a `Token` iterator.
pub struct Lexer<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics> {
	error: PhantomData<E>,
	input: Peekable<I>,
	pos: Position,
	metrics: M
}

impl<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics> Lexer<E, I, M> {
	/// Create a new `Lexer` from an input `char` iterator.
	///
	/// The [`source_span::Metrics`] is used to correctly locate every token in the source text.
	pub fn new(input: I, metrics: M) -> Lexer<E, I, M> {
		Lexer {
			error: PhantomData,
			input: input.peekable(),
			pos: Position::default(),
			metrics
		}
	}
}

fn peek<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, span: &Span, metrics: &M) -> Result<Option<char>, E> {
	match it.peek() {
		Some(Ok(c)) => {
			Ok(Some(*c))
		},
		Some(Err(_)) => {
			let mut dummy_span = *span;
			Ok(consume(it, &mut dummy_span, metrics)?) // always return an error.
		},
		None => Ok(None)
	}
}

fn consume<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, span: &mut Span, metrics: &M) -> Result<Option<char>, E> {
	match it.next() {
		Some(Ok(c)) => {
			span.push(c, metrics);
			Ok(Some(c))
		},
		Some(Err(e)) => Err(Loc::new(Error::IO(e), span.end().into())),
		None => Ok(None)
	}
}

fn parse_group<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, pos: Position, metrics: &M) -> Result<Loc<Token>, E> {
	let mut span = pos.into();

	let delimiter = match consume(it, &mut span, metrics)?.unwrap() {
		'(' => Delimiter::Parenthesis,
		'[' => Delimiter::Bracket,
		_ => unreachable!()
	};

	let mut tokens = Vec::new();
	skip_whitespaces(it, &mut span, metrics)?;

	loop {
		match peek(it, &span, metrics)? {
			Some(')') | Some(']') => {
				let c = consume(it, &mut span, metrics)?.unwrap();
				if c == delimiter.closer() {
					return Ok(Loc::new(Token::Group(delimiter, tokens), span))
				} else {
					return Err(Loc::new(Error::WrongCloser(delimiter, c), span))
				}
			},
			Some(_) => {
				let token = parse_token(it, span.end(), metrics)?.unwrap();
				span.append(token.span());
				tokens.push(token);

				skip_whitespaces(it, &mut span, metrics)?;
			},
			None => return Err(Loc::new(Error::MissingCloser(delimiter), span))
		}
	}
}

fn parse_string<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, pos: Position, metrics: &M) -> Result<Loc<Token>, E> {
	let mut span = pos.into();
	consume(it, &mut span, metrics)?;

	let mut string = String::new();

	loop {
		if let Some(c) = consume(it, &mut span, metrics)? {
			match c {
				'"' => {
					return Ok(Loc::new(Token::String(string), span))
				},
				'\\' => {
					if let Some(c) = consume(it, &mut span, metrics)? {
						match c {
							'r' => string.push('\r'),
							'n' => string.push('\n'),
							't' => string.push('\t'),
							_ => string.push(c)
						}
					} else {
						return Err(Loc::new(Error::UnclosedString, span))
					}
				},
				_ => string.push(c)
			}
		} else {
			return Err(Loc::new(Error::UnclosedString, span))
		}
	}
}

fn parse_iri<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, pos: Position, metrics: &M) -> Result<Loc<Token>, E> {
	let mut span = pos.into();
	consume(it, &mut span, metrics)?;

	let mut string = String::new();

	loop {
		if let Some(c) = consume(it, &mut span, metrics)? {
			match c {
				'>' => {
					break
				},
				_ => string.push(c)
			}
		} else {
			return Err(Loc::new(Error::UnclosedIri, span))
		}
	}

	let iri = match IriBuf::new(&string) {
		Ok(iri) => iri,
		Err(_) => return Err(Loc::new(Error::InvalidIri(string), span))
	};

	Ok(Loc::new(Token::Iri(iri), span))
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

fn parse_ident<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, mut span: Span, metrics: &M) -> Result<Loc<Token>, E> {
	let mut id = String::new();

	loop {
		if let Some(c) = peek(it, &span, metrics)? {
			if !id.is_empty() && is_separator(c) {
				break
			} else {
				consume(it, &mut span, metrics)?;
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

fn parse_numeric<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, sign: bool, mut span: Span, metrics: &M) -> Result<Loc<Token>, E> {
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
		if let Some(c) = peek(it, &span, metrics)? {
			match state {
				State::Integer if c.is_digit(10) => {
					consume(it, &mut span, metrics)?;
					integer = integer * 10 + c.to_digit(10).unwrap() as i64;
				},
				State::Integer if c == '.' => {
					consume(it, &mut span, metrics)?;
					state = State::Decimal;
				}
				State::Decimal if c.is_digit(10) => {
					consume(it, &mut span, metrics)?;
					decimal = decimal * 10 + c.to_digit(10).unwrap() as u32;
				},
				State::Decimal if c == 'e' || c == 'E' => {
					consume(it, &mut span, metrics)?;
					state = State::ExponentSign;
				},
				State::ExponentSign if c == '+' => {
					consume(it, &mut span, metrics)?;
					state = State::ExponentDigit;
				},
				State::ExponentSign if c == '-' => {
					consume(it, &mut span, metrics)?;
					exponent_sign = false;
					state = State::ExponentDigit;
				},
				State::ExponentSign | State::Exponent | State::ExponentDigit if c.is_digit(10) => {
					consume(it, &mut span, metrics)?;
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
		_ => return Err(Loc::new(Error::UnexpectedEos, span))
	};

	Ok(Loc::new(Token::Numeric(n), span))
}

fn skip_whitespaces<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, span: &mut Span, metrics: &M) -> Result<(), E> {
	loop {
		match peek(it, span, metrics)? {
			Some('#') => {
				loop {
					match consume(it, span, metrics)? {
						None | Some('\n') => break,
						_ => ()
					}
				}
			},
			Some(c) if is_space(c) => {
				consume(it, span, metrics)?;
			}
			_ => break
		}
	}

	Ok(())
}

fn parse_token<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics>(it: &mut Peekable<I>, pos: Position, metrics: &M) -> Result<Option<Loc<Token>>, E> {
	let mut whitespace_span = pos.into();
	skip_whitespaces(it, &mut whitespace_span, metrics)?;
	match peek(it, &whitespace_span, metrics)? {
		Some(c) if is_punct(c) => {
			let mut span = whitespace_span.end().into();
			consume(it, &mut span, metrics)?;
			Ok(Some(Loc::new(Token::Punct(c), span)))
		}
		Some('(') | Some('[') => Ok(Some(parse_group(it, whitespace_span.end(), metrics)?)),
		Some(')') | Some(']') => {
			let mut span = whitespace_span.end().into();
			let c = consume(it, &mut span, metrics)?.unwrap();
			Err(Loc::new(Error::Unexpected(c), span))
		},
		Some('"') => Ok(Some(parse_string(it, whitespace_span.end(), metrics)?)),
		Some('<') => Ok(Some(parse_iri(it, whitespace_span.end(), metrics)?)),
		Some('-') => {
			let mut span = whitespace_span.end().into();
			consume(it, &mut span, metrics)?;
			Ok(Some(parse_numeric(it, false, span, metrics)?))
		},
		Some('+') => {
			let mut span = whitespace_span.end().into();
			consume(it, &mut span, metrics)?;
			Ok(Some(parse_numeric(it, true, span, metrics)?))
		},
		Some(c) if c.is_digit(10) => {
			let span = whitespace_span.end().into();
			Ok(Some(parse_numeric(it, true, span, metrics)?))
		},
		Some(_) => {
			let span = whitespace_span.end().into();
			Ok(Some(parse_ident(it, span, metrics)?))
		},
		None => Ok(None)
	}
}

impl<E: std::error::Error, I: Iterator<Item = std::result::Result<char, E>>, M: Metrics> Iterator for Lexer<E, I, M> {
	type Item = Result<Loc<Token>, E>;

	fn next(&mut self) -> Option<Result<Loc<Token>, E>> {
		match parse_token(&mut self.input, self.pos, &self.metrics) {
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
