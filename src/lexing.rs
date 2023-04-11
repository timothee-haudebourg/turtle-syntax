use crate::{BlankIdBuf, DecimalBuf, DoubleBuf, IntegerBuf, NumericLiteral};
use decoded_char::DecodedChar;
use iref::IriRefBuf;
use langtag::LanguageTagBuf;
use locspan::{Meta, Span};
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

/// Fallible tokens iterator with lookahead.
pub trait Tokens {
	type Error;

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Self::Error, Span>>;

	#[allow(clippy::type_complexity)]
	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Self::Error, Span>>;

	/// Returns the span of the last parsed token.
	fn last(&self) -> Span;
}

/// Unexpected char or end of file.
#[derive(Debug, thiserror::Error)]
pub enum Unexpected {
	#[error("unexpected character `{0}`")]
	Char(char),

	#[error("unexpected end of file")]
	EndOfFile,
}

impl From<Option<char>> for Unexpected {
	fn from(value: Option<char>) -> Self {
		match value {
			Some(c) => Self::Char(c),
			None => Self::EndOfFile,
		}
	}
}

/// Lexing error.
#[derive(Debug, thiserror::Error)]
pub enum Error<E = std::convert::Infallible> {
	#[error("invalid language tag")]
	InvalidLangTag,

	#[error("invalid character code point {0:x}")]
	InvalidCodepoint(u32),

	#[error("invalid IRI reference <{0}>: {1}")]
	InvalidIriRef(iref::Error, String),

	#[error(transparent)]
	Unexpected(Unexpected),

	#[error(transparent)]
	Stream(E),
}

/// Token.
#[derive(Debug)]
pub enum Token {
	Keyword(Keyword),
	Begin(Delimiter),
	End(Delimiter),
	LangTag(LanguageTagBuf),
	IriRef(IriRefBuf),
	StringLiteral(String),
	BlankNodeLabel(BlankIdBuf),
	Punct(Punct),
	CompactIri((String, Span), (String, Span)),
	Numeric(NumericLiteral),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Keyword(kw) => write!(f, "keyword `{kw}`"),
			Self::Begin(d) => write!(f, "opening `{}`", d.begin()),
			Self::End(d) => write!(f, "closing `{}`", d.end()),
			Self::LangTag(tag) => write!(f, "language tag `{tag}`"),
			Self::IriRef(iri_ref) => write!(f, "IRI reference <{iri_ref}>"),
			Self::StringLiteral(string) => {
				write!(f, "string literal \"{}\"", DisplayStringLiteral(string))
			}
			Self::BlankNodeLabel(label) => write!(f, "blank node label `{label}`"),
			Self::Punct(p) => p.fmt(f),
			Self::CompactIri((prefix, _), (suffix, _)) => {
				write!(f, "compact IRI `{prefix}:{suffix}`")
			}
			Self::Numeric(n) => write!(f, "numeric literal `{n}`"),
		}
	}
}

/// Wrapper to display string literals.
pub struct DisplayStringLiteral<'a>(pub &'a str);

impl<'a> fmt::Display for DisplayStringLiteral<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for c in self.0.chars() {
			match c {
				'"' => write!(f, "\\u0022"),
				'\\' => write!(f, "\\u005c"),
				'\n' => write!(f, "\\n"),
				'\r' => write!(f, "\\r"),
				'\t' => write!(f, "\\t"),
				'\u{08}' => write!(f, "\\b"),
				'\u{0c}' => write!(f, "\\f"),
				c => c.fmt(f),
			}?
		}

		Ok(())
	}
}

#[derive(Debug)]
pub enum Keyword {
	A,
	Prefix,
	Base,
	SparqlBase,
	SparqlPrefix,
	True,
	False,
}

#[derive(Clone)]
pub struct NotAKeyword;

impl FromStr for Keyword {
	type Err = NotAKeyword;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		if s == "a" {
			Ok(Self::A)
		} else if s == "true" {
			Ok(Self::True)
		} else if s == "false" {
			Ok(Self::False)
		} else if s == unicase::Ascii::new("BASE") {
			Ok(Self::SparqlBase)
		} else if s == unicase::Ascii::new("PREFIX") {
			Ok(Self::SparqlPrefix)
		} else if s == "@prefix" {
			Ok(Self::Prefix)
		} else if s == "@base" {
			Ok(Self::Base)
		} else {
			Err(NotAKeyword)
		}
	}
}

impl fmt::Display for Keyword {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::A => write!(f, "a"),
			Self::Prefix => write!(f, "@prefix"),
			Self::Base => write!(f, "@base"),
			Self::SparqlBase => write!(f, "BASE"),
			Self::SparqlPrefix => write!(f, "PREFIX"),
			Self::True => write!(f, "true"),
			Self::False => write!(f, "false"),
		}
	}
}

#[derive(Debug)]
pub enum Delimiter {
	Parenthesis,
	Bracket,
}

impl Delimiter {
	pub fn begin(&self) -> char {
		match self {
			Self::Parenthesis => '(',
			Self::Bracket => '[',
		}
	}

	pub fn end(&self) -> char {
		match self {
			Self::Parenthesis => ')',
			Self::Bracket => ']',
		}
	}
}

#[derive(Debug)]
pub enum Punct {
	Period,
	Semicolon,
	Comma,
	Carets,
}

impl fmt::Display for Punct {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Period => write!(f, "dot `.`"),
			Self::Semicolon => write!(f, "semicolon `;`"),
			Self::Comma => write!(f, "comma `,`"),
			Self::Carets => write!(f, "carets `^^`"),
		}
	}
}

/// Lexer position.
struct Position {
	span: Span,
	last_span: Span,
}

impl Position {
	fn current(&self) -> Span {
		self.span
	}

	fn end(&self) -> Span {
		self.span.end().into()
	}

	fn last(&self) -> Span {
		self.last_span
	}
}

/// Lexer.
///
/// Changes a character iterator into a `Token` iterator.
pub struct Lexer<C: Iterator<Item = Result<DecodedChar, E>>, E> {
	chars: Peekable<C>,
	pos: Position,
	lookahead: Option<Meta<Token, Span>>,
}

impl<C: Iterator<Item = Result<DecodedChar, E>>, E> Lexer<C, E> {
	pub fn new(chars: C) -> Self {
		Self {
			chars: chars.peekable(),
			pos: Position {
				span: Span::default(),
				last_span: Span::default(),
			},
			lookahead: None,
		}
	}
}

enum LanguageTagOrKeyword {
	Keyword(Keyword),
	LanguageTag(LanguageTagBuf),
}

enum NameOrKeyword {
	Keyword(Keyword),
	CompactIri((String, Span), (String, Span)),
}

enum NumericOrPeriod {
	Numeric(NumericLiteral),
	Period,
}

impl<C: Iterator<Item = Result<DecodedChar, E>>, E> Lexer<C, E> {
	fn peek_char(&mut self) -> Result<Option<char>, Meta<Error<E>, Span>> {
		match self.chars.peek() {
			None => Ok(None),
			Some(Ok(c)) => Ok(Some(c.chr())),
			Some(Err(_)) => self.next_char(),
		}
	}

	fn next_char(&mut self) -> Result<Option<char>, Meta<Error<E>, Span>> {
		match self.chars.next() {
			None => Ok(None),
			Some(Ok(c)) => {
				self.pos.span.push(c.len());
				self.pos.last_span.clear();
				self.pos.last_span.push(c.len());
				Ok(Some(c.chr()))
			}
			Some(Err(e)) => Err(Meta(Error::Stream(e), self.pos.end())),
		}
	}

	fn expect_char(&mut self) -> Result<char, Meta<Error<E>, Span>> {
		self.next_char()?
			.ok_or_else(|| Meta(Error::Unexpected(Unexpected::EndOfFile), self.pos.end()))
	}

	fn skip_whitespaces(&mut self) -> Result<(), Meta<Error<E>, Span>> {
		while let Some(c) = self.peek_char()? {
			if c.is_whitespace() {
				self.next_char()?;
			} else if c == '#' {
				self.next_comment()?;
			} else {
				break;
			}
		}

		self.pos.span.clear();
		Ok(())
	}

	/// Parses the rest of a comment, after the first `#` character.
	///
	/// Comments in N-Quads take the form of `#`,
	/// outside an IRIREF or STRING_LITERAL_QUOTE,
	/// and continue to the end of line (EOL) or end of file
	/// if there is no end of line after the comment marker.
	fn next_comment(&mut self) -> Result<(), Meta<Error<E>, Span>> {
		loop {
			if matches!(self.next_char()?, None | Some('\n')) {
				break Ok(());
			}
		}
	}

	/// Parses the rest of a lang tag, after the first `@` character.
	fn next_langtag_or_keyword(
		&mut self,
	) -> Result<Meta<LanguageTagOrKeyword, Span>, Meta<Error<E>, Span>> {
		let mut tag = String::new();

		loop {
			match self.peek_char()? {
				None => {
					if tag.is_empty() {
						return Err(Meta(Error::InvalidLangTag, self.pos.current()));
					} else {
						break;
					}
				}
				Some(c) => {
					if c.is_ascii_alphabetic() {
						tag.push(self.expect_char()?);
					} else if tag.is_empty() {
						return Err(Meta(Error::InvalidLangTag, self.pos.current()));
					} else {
						break;
					}
				}
			}
		}

		let mut empty_subtag = true;
		if let Some('-') = self.peek_char()? {
			tag.push(self.expect_char()?);
			loop {
				match self.peek_char()? {
					Some('-') if !empty_subtag => tag.push(self.expect_char()?),
					Some(c) if c.is_ascii_alphanumeric() => {
						empty_subtag = false;
						tag.push(self.expect_char()?)
					}
					Some(c) => {
						if c.is_whitespace() {
							if empty_subtag {
								return Err(Meta(Error::InvalidLangTag, self.pos.current()));
							} else {
								break;
							}
						} else {
							self.next_char()?;
							return Err(Meta(
								Error::Unexpected(Unexpected::Char(c)),
								self.pos.last(),
							));
						}
					}
					None => {
						if empty_subtag {
							return Err(Meta(Error::InvalidLangTag, self.pos.current()));
						} else {
							break;
						}
					}
				}
			}
		}

		match tag.as_str() {
			"prefix" => Ok(Meta(
				LanguageTagOrKeyword::Keyword(Keyword::Prefix),
				self.pos.current(),
			)),
			"base" => Ok(Meta(
				LanguageTagOrKeyword::Keyword(Keyword::Base),
				self.pos.current(),
			)),
			_ => match LanguageTagBuf::new(tag.into_bytes()) {
				Ok(tag) => Ok(Meta(
					LanguageTagOrKeyword::LanguageTag(tag),
					self.pos.current(),
				)),
				Err(_) => Err(Meta(Error::InvalidLangTag, self.pos.current())),
			},
		}
	}

	/// Parses an IRI reference, starting after the first `<` until the closing
	/// `>`.
	fn next_iriref(&mut self) -> Result<Meta<IriRefBuf, Span>, Meta<Error<E>, Span>> {
		let mut iriref = String::new();

		loop {
			match self.next_char()? {
				Some('>') => break,
				Some('\\') => {
					let span = self.pos.last();
					let c = match self.next_char()? {
						Some('u') => self.next_hex_char(span, 4)?,
						Some('U') => self.next_hex_char(span, 8)?,
						unexpected => {
							return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
						}
					};

					iriref.push(c)
				}
				Some(c) => {
					if matches!(
						c,
						'\u{00}'..='\u{20}' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' | '\\'
					) {
						return Err(Meta(
							Error::Unexpected(Unexpected::Char(c)),
							self.pos.last(),
						));
					}

					iriref.push(c)
				}
				None => {
					return Err(Meta(
						Error::Unexpected(Unexpected::EndOfFile),
						self.pos.end(),
					))
				}
			}
		}

		match IriRefBuf::from_string(iriref) {
			Ok(iriref) => Ok(Meta(iriref, self.pos.current())),
			Err((e, string)) => Err(Meta(Error::InvalidIriRef(e, string), self.pos.current())),
		}
	}

	fn next_hex_char(&mut self, mut span: Span, len: u8) -> Result<char, Meta<Error<E>, Span>> {
		let mut codepoint = 0;

		for _ in 0..len {
			let c = self.expect_char()?;
			match c.to_digit(16) {
				Some(d) => codepoint = codepoint << 4 | d,
				None => {
					return Err(Meta(
						Error::Unexpected(Unexpected::Char(c)),
						self.pos.last(),
					))
				}
			}
		}

		span.set_end(self.pos.current().end());
		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(Meta(Error::InvalidCodepoint(codepoint), span)),
		}
	}

	/// Parses a string literal, starting after the first `"` until the closing
	/// `"`.
	fn next_string_literal(
		&mut self,
		delimiter: char,
	) -> Result<Meta<String, Span>, Meta<Error<E>, Span>> {
		let mut string = String::new();

		let mut long = false;

		loop {
			match self.next_char()? {
				Some(c) if c == delimiter => {
					if !long {
						if string.is_empty() && self.peek_char()? == Some(delimiter) {
							self.next_char()?;
							long = true;
						} else {
							break;
						}
					} else if self.peek_char()? == Some(delimiter) {
						self.next_char()?;
						if self.peek_char()? == Some(delimiter) {
							self.next_char()?;
							break;
						} else {
							string.push(delimiter);
							string.push(delimiter);
						}
					} else {
						string.push(delimiter);
					}
				}
				Some('\\') => {
					let span = self.pos.last();
					let c = match self.next_char()? {
						Some('u') => self.next_hex_char(span, 4)?,
						Some('U') => self.next_hex_char(span, 8)?,
						Some('t') => '\t',
						Some('b') => '\u{08}',
						Some('n') => '\n',
						Some('r') => '\r',
						Some('f') => '\u{0c}',
						Some('\'') => '\'',
						Some('"') => '"',
						Some('\\') => '\\',
						unexpected => {
							return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
						}
					};

					string.push(c)
				}
				Some(c) => {
					// if !long && matches!(c, '\n' | '\r') {
					// 	return Err(Meta(Error::Unexpected(Unexpected::Char(c)), self.pos.last()));
					// }
					string.push(c)
				}
				None => {
					return Err(Meta(
						Error::Unexpected(Unexpected::EndOfFile),
						self.pos.end(),
					))
				}
			}
		}

		Ok(Meta(string, self.pos.current()))
	}

	/// Parses an IRI reference, starting after the first `<` until the closing
	/// `>`.
	fn next_numeric_or_dot(
		&mut self,
		first: char,
	) -> Result<Meta<NumericOrPeriod, Span>, Meta<Error<E>, Span>> {
		let mut buffer: String = first.into();

		enum State {
			NonEmptyInteger,
			Integer,
			NonENonEmptyDecimal,
			NonEmptyDecimal,
			Decimal,
			ExponentSign,
			NonEmptyExponent,
			Exponent,
		}

		let mut state = match first {
			'+' => State::NonEmptyInteger,
			'-' => State::NonEmptyInteger,
			'.' => State::NonENonEmptyDecimal,
			'0'..='9' => State::Integer,
			_ => panic!("invalid first numeric character"),
		};

		loop {
			state = match state {
				State::NonEmptyInteger => match self.peek_char()? {
					Some('0'..='9') => State::Integer,
					Some('.') => State::NonEmptyDecimal,
					unexpected => {
						return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
					}
				},
				State::Integer => match self.peek_char()? {
					Some('0'..='9') => State::Integer,
					Some('.') => State::NonEmptyDecimal,
					Some('e' | 'E') => State::ExponentSign,
					_ => break,
				},
				State::NonENonEmptyDecimal => match self.peek_char()? {
					Some('0'..='9') => State::Decimal,
					_ => return Ok(Meta(NumericOrPeriod::Period, self.pos.current())),
				},
				State::NonEmptyDecimal => match self.peek_char()? {
					Some('0'..='9') => State::Decimal,
					Some('e' | 'E') => State::ExponentSign,
					unexpected => {
						return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
					}
				},
				State::Decimal => match self.peek_char()? {
					Some('0'..='9') => State::Decimal,
					Some('e' | 'E') => State::ExponentSign,
					_ => break,
				},
				State::ExponentSign => match self.peek_char()? {
					Some('+' | '-') => State::NonEmptyExponent,
					Some('0'..='9') => State::Exponent,
					unexpected => {
						return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
					}
				},
				State::NonEmptyExponent => match self.peek_char()? {
					Some('0'..='9') => State::Exponent,
					unexpected => {
						return Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last()))
					}
				},
				State::Exponent => match self.peek_char()? {
					Some('0'..='9') => State::Exponent,
					_ => break,
				},
			};

			buffer.push(self.expect_char()?);
		}

		let n = match state {
			State::Integer => NumericLiteral::Integer(unsafe { IntegerBuf::new_unchecked(buffer) }),
			State::Decimal => NumericLiteral::Decimal(unsafe { DecimalBuf::new_unchecked(buffer) }),
			State::Exponent => NumericLiteral::Double(unsafe { DoubleBuf::new_unchecked(buffer) }),
			_ => unreachable!(),
		};

		Ok(Meta(NumericOrPeriod::Numeric(n), self.pos.current()))
	}

	/// Parses a blank node label, starting after the first `_`.
	fn next_blank_node_label(&mut self) -> Result<Meta<BlankIdBuf, Span>, Meta<Error<E>, Span>> {
		match self.next_char()? {
			Some(':') => {
				let mut label = String::new();
				label.push('_');
				label.push(':');
				match self.next_char()? {
					Some(c) if c.is_ascii_digit() || is_pn_chars_u(c) => {
						label.push(c);
						let mut last_is_pn_chars = true;
						loop {
							match self.peek_char()? {
								Some(c) if is_pn_chars(c) => {
									label.push(self.expect_char()?);
									last_is_pn_chars = true
								}
								Some('.') => {
									label.push(self.expect_char()?);
									last_is_pn_chars = false;
								}
								_ if last_is_pn_chars => break,
								unexpected => {
									return Err(Meta(
										Error::Unexpected(unexpected.into()),
										self.pos.last(),
									))
								}
							}
						}

						Ok(Meta(
							unsafe { BlankIdBuf::new_unchecked(label) },
							self.pos.current(),
						))
					}
					unexpected => Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last())),
				}
			}
			unexpected => Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last())),
		}
	}

	fn next_escape(&mut self) -> Result<char, Meta<Error<E>, Span>> {
		match self.next_char()? {
			Some(
				c @ ('_' | '~' | '.' | '-' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
				| ';' | '=' | '/' | '?' | '#' | '@' | '%'),
			) => Ok(c),
			unexpected => Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last())),
		}
	}

	fn next_name_or_keyword(
		&mut self,
		c: char,
	) -> Result<Meta<NameOrKeyword, Span>, Meta<Error<E>, Span>> {
		// PNAME_NS or Keyword
		let namespace = match c {
			':' => (String::new(), self.pos.current()),
			c if is_pn_chars_base(c) => {
				let mut namespace = String::new();
				namespace.push(c);
				let mut last_is_pn_chars = true;
				let span = loop {
					match self.peek_char()? {
						Some(c) if is_pn_chars(c) => {
							namespace.push(self.expect_char()?);
							last_is_pn_chars = true
						}
						Some('.') => {
							namespace.push(self.expect_char()?);
							last_is_pn_chars = false;
						}
						Some(':') if last_is_pn_chars => {
							let span = self.pos.current();
							self.expect_char()?;
							break span;
						}
						unexpected => {
							return if unexpected.map(|c| c.is_whitespace()).unwrap_or(true) {
								match Keyword::from_str(&namespace) {
									Ok(kw) => {
										Ok(Meta(NameOrKeyword::Keyword(kw), self.pos.current()))
									}
									Err(NotAKeyword) => break self.pos.current(),
								}
							} else {
								Err(Meta(Error::Unexpected(unexpected.into()), self.pos.end()))
							}
						}
					}
				};

				(namespace, span)
			}
			unexpected => {
				return Err(Meta(
					Error::Unexpected(Unexpected::Char(unexpected)),
					self.pos.last(),
				))
			}
		};

		// PN_LOCAL
		let mut suffix = String::new();
		let mut suffix_span = self.pos.current().next();
		match self.peek_char()? {
			Some(c) if is_pn_chars_u(c) || c.is_ascii_digit() || matches!(c, ':' | '%' | '\\') => {
				let c = match self.expect_char()? {
					'%' => {
						// percent encoded.
						self.next_hex_char(self.pos.current().end().into(), 2)?
					}
					'\\' => {
						// escape sequence.
						self.next_escape()?
					}
					c => c,
				};

				suffix.push(c);

				loop {
					match self.peek_char()? {
						Some(c)
							if is_pn_chars(c)
								|| c.is_ascii_digit() || matches!(c, ':' | '%' | '\\') =>
						{
							let c = match self.expect_char()? {
								'%' => {
									// percent encoded.
									self.next_hex_char(self.pos.current().end().into(), 2)?
								}
								'\\' => {
									// escape sequence.
									self.next_escape()?
								}
								c => c,
							};

							suffix.push(c);
						}
						_ => {
							suffix_span.set_end(self.pos.current().end());
							break Ok(Meta(
								NameOrKeyword::CompactIri(namespace, (suffix, suffix_span)),
								self.pos.current(),
							));
						}
					}
				}
			}
			_ => Ok(Meta(
				NameOrKeyword::CompactIri(namespace, (String::new(), self.pos.current())),
				self.pos.current(),
			)),
		}
	}

	pub fn consume(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		self.skip_whitespaces()?;
		match self.next_char()? {
			Some('@') => Ok(self.next_langtag_or_keyword()?.map(|t| match t {
				LanguageTagOrKeyword::LanguageTag(tag) => Some(Token::LangTag(tag)),
				LanguageTagOrKeyword::Keyword(kw) => Some(Token::Keyword(kw)),
			})),
			Some('<') => Ok(self.next_iriref()?.map(|t| Some(Token::IriRef(t)))),
			Some('"') => Ok(self
				.next_string_literal('"')?
				.map(|t| Some(Token::StringLiteral(t)))),
			Some('\'') => Ok(self
				.next_string_literal('\'')?
				.map(|t| Some(Token::StringLiteral(t)))),
			Some('_') => Ok(self
				.next_blank_node_label()?
				.map(|t| Some(Token::BlankNodeLabel(t)))),
			Some(',') => Ok(Meta(Some(Token::Punct(Punct::Comma)), self.pos.current())),
			Some(';') => Ok(Meta(
				Some(Token::Punct(Punct::Semicolon)),
				self.pos.current(),
			)),
			Some('^') => match self.next_char()? {
				Some('^') => Ok(Meta(Some(Token::Punct(Punct::Carets)), self.pos.current())),
				unexpected => Err(Meta(Error::Unexpected(unexpected.into()), self.pos.last())),
			},
			Some('(') => Ok(Meta(
				Some(Token::Begin(Delimiter::Parenthesis)),
				self.pos.current(),
			)),
			Some('[') => Ok(Meta(
				Some(Token::Begin(Delimiter::Bracket)),
				self.pos.current(),
			)),
			Some(')') => Ok(Meta(
				Some(Token::End(Delimiter::Parenthesis)),
				self.pos.current(),
			)),
			Some(']') => Ok(Meta(
				Some(Token::End(Delimiter::Bracket)),
				self.pos.current(),
			)),
			Some(c @ ('+' | '-' | '0'..='9' | '.')) => {
				Ok(self.next_numeric_or_dot(c)?.map(|t| match t {
					NumericOrPeriod::Numeric(n) => Some(Token::Numeric(n)),
					NumericOrPeriod::Period => Some(Token::Punct(Punct::Period)),
				}))
			}
			Some(c) => Ok(self.next_name_or_keyword(c)?.map(|t| match t {
				NameOrKeyword::Keyword(kw) => Some(Token::Keyword(kw)),
				NameOrKeyword::CompactIri(p, s) => Some(Token::CompactIri(p, s)),
			})),
			None => Ok(Meta(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity)]
	pub fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Error<E>, Span>> {
		if self.lookahead.is_none() {
			if let locspan::Meta(Some(token), loc) = self.consume()? {
				self.lookahead = Some(Meta::new(token, loc));
			}
		}

		match &self.lookahead {
			Some(locspan::Meta(token, loc)) => Ok(Meta::new(Some(token), *loc)),
			None => Ok(Meta::new(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity, clippy::should_implement_trait)]
	pub fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		match self.lookahead.take() {
			Some(locspan::Meta(token, loc)) => Ok(Meta::new(Some(token), loc)),
			None => self.consume(),
		}
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Tokens for Lexer<C, E> {
	type Error = Error<E>;

	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, Meta<Error<E>, Span>> {
		self.peek()
	}

	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, Meta<Error<E>, Span>> {
		self.next()
	}

	fn last(&self) -> Span {
		self.pos.last_span
	}
}

impl<E, C: Iterator<Item = Result<DecodedChar, E>>> Iterator for Lexer<C, E> {
	type Item = Result<Meta<Token, Span>, Meta<Error<E>, Span>>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.next() {
			Ok(Meta(Some(token), loc)) => Some(Ok(Meta::new(token, loc))),
			Ok(Meta(None, _)) => None,
			Err(e) => Some(Err(e)),
		}
	}
}

fn is_pn_chars_base(c: char) -> bool {
	matches!(c, 'A'..='Z' | 'a'..='z' | '\u{00c0}'..='\u{00d6}' | '\u{00d8}'..='\u{00f6}' | '\u{00f8}'..='\u{02ff}' | '\u{0370}'..='\u{037d}' | '\u{037f}'..='\u{1fff}' | '\u{200c}'..='\u{200d}' | '\u{2070}'..='\u{218f}' | '\u{2c00}'..='\u{2fef}' | '\u{3001}'..='\u{d7ff}' | '\u{f900}'..='\u{fdcf}' | '\u{fdf0}'..='\u{fffd}' | '\u{10000}'..='\u{effff}')
}

fn is_pn_chars_u(c: char) -> bool {
	is_pn_chars_base(c) || c == '_'
}

fn is_pn_chars(c: char) -> bool {
	is_pn_chars_u(c)
		|| matches!(c, '-' | '0'..='9' | '\u{00b7}' | '\u{0300}'..='\u{036f}' | '\u{203f}'..='\u{2040}')
}
