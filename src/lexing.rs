use iref::IriRefBuf;
use langtag::LanguageTagBuf;
use locspan::{Loc, Location, Span};
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

/// Changes a `char` iterator into a `DecodedChar` iterator using each character
/// UTF-8 encoded length.
pub struct Utf8Decoded<C>(C);

impl<C> Utf8Decoded<C> {
	pub fn new(chars: C) -> Self { Self(chars) }
}

impl<E, C: Iterator<Item = Result<char, E>>> Iterator for Utf8Decoded<C> {
	type Item = Result<DecodedChar, E>;

	fn next(&mut self) -> Option<Self::Item> {
		self.0
			.next()
			.map(|r| r.map(|c| DecodedChar::new(c, c.len_utf8())))
	}
}

/// Decoded character, with its encoded byte length.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct DecodedChar {
	c: char,
	len: usize,
}

impl DecodedChar {
	/// Creates a new decoded character.
	#[inline(always)]
	pub fn new(c: char, len: usize) -> DecodedChar { Self { c, len } }

	/// Unwraps and returns the character.
	#[inline(always)]
	pub fn unwrap(self) -> char { self.c }

	/// Returns the original encoded byte length of the character.
	#[inline(always)]
	#[allow(clippy::len_without_is_empty)]
	pub fn len(&self) -> usize { self.len }
}

impl From<DecodedChar> for char {
	fn from(e: DecodedChar) -> Self { e.c }
}

impl std::ops::Deref for DecodedChar {
	type Target = char;

	fn deref(&self) -> &char { &self.c }
}

/// Stream of token, with lookahead.
pub trait Tokens<F> {
	type Error: fmt::Debug;

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Self::Error, F>>;

	#[allow(clippy::type_complexity)]
	fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Self::Error, F>>;
}

/// Lexing error.
#[derive(Debug)]
pub enum Error<E> {
	InvalidLangTag,
	InvalidCodepoint(u32),
	InvalidIriRef(iref::Error, String),
	Unexpected(Option<char>),
	Stream(E),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::InvalidLangTag => write!(f, "invalid language tag"),
			Self::InvalidCodepoint(c) => write!(f, "invalid character code point {:x}", c),
			Self::InvalidIriRef(e, iri_ref) => {
				write!(f, "invalid IRI reference <{}>: {}", iri_ref, e)
			}
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(c)) => write!(f, "unexpected character `{}`", c),
			Self::Stream(e) => e.fmt(f),
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for Error<E> {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::InvalidIriRef(e, _) => Some(e),
			Self::Stream(e) => Some(e),
			_ => None,
		}
	}
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
	BlankNodeLabel(String),
	Punct(Punct),
	Namespace(String),
	CompactIri(Option<(String, Span)>, (String, Span)),
	Numeric(Numeric),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Keyword(kw) => write!(f, "keyword `{}`", kw),
			Self::Begin(d) => write!(f, "opening `{}`", d.begin()),
			Self::End(d) => write!(f, "closing `{}`", d.end()),
			Self::LangTag(tag) => write!(f, "language tag `{}`", tag),
			Self::IriRef(iri_ref) => write!(f, "IRI reference <{}>", iri_ref),
			Self::StringLiteral(string) => {
				write!(f, "string literal \"{}\"", DisplayStringLiteral(string))
			}
			Self::BlankNodeLabel(label) => write!(f, "blank node label `{}`", label),
			Self::Punct(p) => p.fmt(f),
			Self::Namespace(ns) => write!(f, "namespace `{}`", ns),
			Self::CompactIri(None, (suffix, _)) => write!(f, "compact IRI `:{}`", suffix),
			Self::CompactIri(Some((prefix, _)), (suffix, _)) => {
				write!(f, "compact IRI `{}:{}`", prefix, suffix)
			}
			Self::Numeric(n) => write!(f, "numeric literal `{}`", n),
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

/// Numerical value.
#[derive(Clone, Debug)]
pub struct Numeric(String);

impl fmt::Display for Numeric {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.0.fmt(f) }
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
	Dot,
	Semicolon,
	Comma,
	Carets,
}

impl fmt::Display for Punct {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Dot => write!(f, "dot `.`"),
			Self::Semicolon => write!(f, "semicolon `;`"),
			Self::Comma => write!(f, "comma `,`"),
			Self::Carets => write!(f, "carets `^^`"),
		}
	}
}

/// Lexer position.
struct Position<F> {
	file: F,
	span: Span,
	last_span: Span,
}

impl<F: Clone> Position<F> {
	fn current(&self) -> Location<F> { Location::new(self.file.clone(), self.span) }

	fn current_span(&self) -> Span { self.span }

	fn end(&self) -> Location<F> { Location::new(self.file.clone(), self.span.end()) }

	fn last(&self) -> Location<F> { Location::new(self.file.clone(), self.last_span) }

	fn last_span(&self) -> Span { self.last_span }
}

/// Lexer.
///
/// Changes a character iterator into a `Token` iterator.
pub struct Lexer<F, E, C: Iterator<Item = Result<DecodedChar, E>>> {
	chars: Peekable<C>,
	pos: Position<F>,
	lookahead: Option<Loc<Token, F>>,
}

impl<F, E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<F, E, C> {
	pub fn new(file: F, chars: Peekable<C>) -> Self {
		Self {
			chars,
			pos: Position {
				file,
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
	Namespace(String),
	CompactIri(Option<(String, Span)>, (String, Span)),
}

enum NumericOrDot {
	Numeric(Numeric),
	Dot,
}

impl<F: Clone, E, C: Iterator<Item = Result<DecodedChar, E>>> Lexer<F, E, C> {
	fn peek_char(&mut self) -> Result<Option<char>, Loc<Error<E>, F>> {
		match self.chars.peek() {
			None => Ok(None),
			Some(Ok(c)) => Ok(Some(c.unwrap())),
			Some(Err(_)) => self.next_char(),
		}
	}

	fn next_char(&mut self) -> Result<Option<char>, Loc<Error<E>, F>> {
		match self.chars.next() {
			None => Ok(None),
			Some(Ok(c)) => {
				self.pos.span.push(c.len());
				self.pos.last_span.clear();
				self.pos.last_span.push(c.len());
				Ok(Some(c.unwrap()))
			}
			Some(Err(e)) => Err(Loc(Error::Stream(e), self.pos.end())),
		}
	}

	fn expect_char(&mut self) -> Result<char, Loc<Error<E>, F>> {
		self.next_char()?
			.ok_or_else(|| Loc(Error::Unexpected(None), self.pos.end()))
	}

	fn skip_whitespaces(&mut self) -> Result<(), Loc<Error<E>, F>> {
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
	fn next_comment(&mut self) -> Result<(), Loc<Error<E>, F>> {
		loop {
			if matches!(self.next_char()?, None | Some('\n')) {
				break Ok(());
			}
		}
	}

	/// Parses the rest of a lang tag, after the first `@` character.
	fn next_langtag_or_keyword(
		&mut self,
	) -> Result<Loc<LanguageTagOrKeyword, F>, Loc<Error<E>, F>> {
		let mut tag = String::new();
		
		loop {
			match self.peek_char()? {
				None => {
					if tag.is_empty() {
						return Err(Loc(Error::InvalidLangTag, self.pos.current()));
					} else {
						break;
					}
				}
				Some(c) => {
					if c.is_ascii_alphabetic() {
						tag.push(self.expect_char()?);
					} else if c.is_whitespace() {
						break
					} else {
						self.next_char()?;
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}
				}
			}
		}

		while let Some('-') = self.peek_char()? {
			loop {
				match self.peek_char()? {
					None => {
						if tag.is_empty() {
							return Err(Loc(Error::InvalidLangTag, self.pos.current()));
						} else {
							break;
						}
					}
					Some(c) => {
						if c.is_ascii_alphanumeric() {
							tag.push(self.expect_char()?);
						} else {
							return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
						}
					}
				}
			}
		}

		match tag.as_str() {
			"prefix" => Ok(Loc(
				LanguageTagOrKeyword::Keyword(Keyword::Prefix),
				self.pos.current(),
			)),
			"base" => Ok(Loc(
				LanguageTagOrKeyword::Keyword(Keyword::Base),
				self.pos.current(),
			)),
			_ => match LanguageTagBuf::new(tag.into_bytes()) {
				Ok(tag) => Ok(Loc(
					LanguageTagOrKeyword::LanguageTag(tag),
					self.pos.current(),
				)),
				Err(_) => Err(Loc(Error::InvalidLangTag, self.pos.current())),
			},
		}
	}

	/// Parses an IRI reference, starting after the first `<` until the closing
	/// `>`.
	fn next_iriref(&mut self) -> Result<Loc<IriRefBuf, F>, Loc<Error<E>, F>> {
		let mut iriref = String::new();

		loop {
			match self.next_char()? {
				Some('>') => break,
				Some('\\') => {
					let span = self.pos.last_span();
					let c = match self.next_char()? {
						Some('u') => self.next_hex_char(span, 4)?,
						Some('U') => self.next_hex_char(span, 8)?,
						unexpected => {
							return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					iriref.push(c)
				}
				Some(c) => {
					if matches!(
						c,
						'\u{00}'..='\u{20}' | '<' | '>' | '"' | '{' | '}' | '|' | '^' | '`' | '\\'
					) {
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}

					iriref.push(c)
				}
				None => return Err(Loc(Error::Unexpected(None), self.pos.end())),
			}
		}

		match IriRefBuf::from_string(iriref) {
			Ok(iriref) => Ok(Loc(iriref, self.pos.current())),
			Err((e, string)) => Err(Loc(Error::InvalidIriRef(e, string), self.pos.current())),
		}
	}

	fn next_hex_char(&mut self, mut span: Span, len: u8) -> Result<char, Loc<Error<E>, F>> {
		let mut codepoint = 0;

		for _ in 0..len {
			let c = self.expect_char()?;
			match c.to_digit(16) {
				Some(d) => codepoint = codepoint << 4 | d,
				None => return Err(Loc(Error::Unexpected(Some(c)), self.pos.last())),
			}
		}

		span.set_end(self.pos.current_span().end());
		match char::try_from(codepoint) {
			Ok(c) => Ok(c),
			Err(_) => Err(Loc(
				Error::InvalidCodepoint(codepoint),
				Location::new(self.pos.file.clone(), span),
			)),
		}
	}

	/// Parses a string literal, starting after the first `"` until the closing
	/// `"`.
	fn next_string_literal(&mut self, delimiter: char) -> Result<Loc<String, F>, Loc<Error<E>, F>> {
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
					let span = self.pos.last_span();
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
							return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
						}
					};

					string.push(c)
				}
				Some(c) => {
					if matches!(c, '\n' | '\r') {
						return Err(Loc(Error::Unexpected(Some(c)), self.pos.last()));
					}

					string.push(c)
				}
				None => return Err(Loc(Error::Unexpected(None), self.pos.end())),
			}
		}

		Ok(Loc(string, self.pos.current()))
	}

	/// Parses an IRI reference, starting after the first `<` until the closing
	/// `>`.
	fn next_numeric_or_dot(
		&mut self,
		first: char,
	) -> Result<Loc<NumericOrDot, F>, Loc<Error<E>, F>> {
		let mut buffer = String::new();

		enum State {
			Integer,
			NonEmptyDecimal,
			Decimal,
			ExponentSign,
			NonEmptyExponent,
			Exponent,
		}

		let mut state = match first {
			'+' => State::Integer,
			'-' => State::Integer,
			'.' => State::NonEmptyDecimal,
			'0'..='9' => State::Integer,
			_ => panic!("invalid first numeric character"),
		};

		loop {
			match state {
				State::Integer => match self.peek_char()? {
					Some('0'..='9') => (),
					Some('.') => state = State::Decimal,
					Some('e' | 'E') => state = State::ExponentSign,
					_ => break,
				},
				State::NonEmptyDecimal => match self.peek_char()? {
					Some('0'..='9') => state = State::Decimal,
					Some('e' | 'E') => state = State::ExponentSign,
					_ => return Ok(Loc(NumericOrDot::Dot, self.pos.current())),
				},
				State::Decimal => match self.peek_char()? {
					Some('0'..='9') => (),
					Some('e' | 'E') => state = State::ExponentSign,
					_ => break,
				},
				State::ExponentSign => match self.peek_char()? {
					Some('+' | '-') => state = State::Exponent,
					Some('0'..='9') => state = State::NonEmptyExponent,
					unexpected => return Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
				},
				State::NonEmptyExponent => match self.peek_char()? {
					Some('0'..='9') => state = State::Decimal,
					Some('e' | 'E') => state = State::ExponentSign,
					unexpected => return Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
				},
				State::Exponent => match self.peek_char()? {
					Some('0'..='9') => state = State::Exponent,
					_ => break,
				},
			}

			buffer.push(self.expect_char()?);
		}

		Ok(Loc(
			NumericOrDot::Numeric(Numeric(buffer)),
			self.pos.current(),
		))
	}

	/// Parses a blank node label, starting after the first `_`.
	fn next_blank_node_label(&mut self) -> Result<Loc<String, F>, Loc<Error<E>, F>> {
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
									return Err(Loc(Error::Unexpected(unexpected), self.pos.last()))
								}
							}
						}

						Ok(Loc(label, self.pos.current()))
					}
					unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
				}
			}
			unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	fn next_escape(&mut self) -> Result<char, Loc<Error<E>, F>> {
		match self.next_char()? {
			Some(
				c @ ('_' | '~' | '.' | '-' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
				| ';' | '=' | '/' | '?' | '#' | '@' | '%'),
			) => Ok(c),
			unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
		}
	}

	fn next_name_or_keyword(&mut self, c: char) -> Result<Loc<NameOrKeyword, F>, Loc<Error<E>, F>> {
		// PNAME_NS or Keyword
		let namespace = match c {
			':' => None,
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
							let span = self.pos.current_span();
							self.expect_char()?;
							break span;
						}
						unexpected => {
							return if unexpected.map(|c| c.is_whitespace()).unwrap_or(true) {
								match Keyword::from_str(&namespace) {
									Ok(kw) => {
										Ok(Loc(NameOrKeyword::Keyword(kw), self.pos.current()))
									}
									Err(NotAKeyword) => {
										break self.pos.current_span()
									}
								}
							} else {
								Err(Loc(Error::Unexpected(unexpected), self.pos.end()))
							}
						}
					}
				};

				Some((namespace, span))
			}
			unexpected => return Err(Loc(Error::Unexpected(Some(unexpected)), self.pos.last())),
		};

		// PN_LOCAL
		let mut suffix = String::new();
		let mut suffix_span = self.pos.current_span().next();
		match self.peek_char()? {
			Some(c) if is_pn_chars_u(c) || c.is_ascii_digit() || matches!(c, ':' | '%' | '\\') => {
				let c = match self.expect_char()? {
					'%' => {
						// percent encoded.
						self.next_hex_char(self.pos.current_span().end().into(), 2)?
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
									self.next_hex_char(self.pos.current_span().end().into(), 2)?
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
							suffix_span.set_end(self.pos.current_span().end());
							break Ok(Loc(
								NameOrKeyword::CompactIri(namespace, (suffix, suffix_span)),
								self.pos.current(),
							));
						}
					}
				}
			}
			_ => match namespace {
				Some((namespace, _)) => Ok(Loc(NameOrKeyword::Namespace(namespace), self.pos.current())),
				None => Ok(Loc(NameOrKeyword::Namespace(String::new()), self.pos.current()))
			},
		}
	}

	pub fn consume(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> {
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
			Some(',') => Ok(Loc(Some(Token::Punct(Punct::Comma)), self.pos.current())),
			Some(';') => Ok(Loc(
				Some(Token::Punct(Punct::Semicolon)),
				self.pos.current(),
			)),
			Some('^') => match self.next_char()? {
				Some('^') => Ok(Loc(Some(Token::Punct(Punct::Carets)), self.pos.current())),
				unexpected => Err(Loc(Error::Unexpected(unexpected), self.pos.last())),
			},
			Some('(') => Ok(Loc(
				Some(Token::Begin(Delimiter::Parenthesis)),
				self.pos.current(),
			)),
			Some('[') => Ok(Loc(
				Some(Token::Begin(Delimiter::Bracket)),
				self.pos.current(),
			)),
			Some(')') => Ok(Loc(
				Some(Token::End(Delimiter::Parenthesis)),
				self.pos.current(),
			)),
			Some(']') => Ok(Loc(
				Some(Token::End(Delimiter::Bracket)),
				self.pos.current(),
			)),
			Some(c @ ('+' | '-' | '0'..='9' | '.')) => {
				Ok(self.next_numeric_or_dot(c)?.map(|t| match t {
					NumericOrDot::Numeric(n) => Some(Token::Numeric(n)),
					NumericOrDot::Dot => Some(Token::Punct(Punct::Dot)),
				}))
			}
			Some(c) => Ok(self.next_name_or_keyword(c)?.map(|t| match t {
				NameOrKeyword::Keyword(kw) => Some(Token::Keyword(kw)),
				NameOrKeyword::Namespace(n) => Some(Token::Namespace(n)),
				NameOrKeyword::CompactIri(p, s) => Some(Token::CompactIri(p, s)),
			})),
			None => Ok(Loc(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity)]
	pub fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Error<E>, F>> {
		if self.lookahead.is_none() {
			if let locspan::Loc(Some(token), loc) = self.consume()? {
				self.lookahead = Some(Loc::new(token, loc));
			}
		}

		match &self.lookahead {
			Some(locspan::Loc(token, loc)) => Ok(Loc::new(Some(token), loc.clone())),
			None => Ok(Loc::new(None, self.pos.end())),
		}
	}

	#[allow(clippy::type_complexity, clippy::should_implement_trait)]
	pub fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> {
		match self.lookahead.take() {
			Some(locspan::Loc(token, loc)) => Ok(Loc::new(Some(token), loc)),
			None => self.consume(),
		}
	}
}

impl<F: Clone, E: fmt::Debug, C: Iterator<Item = Result<DecodedChar, E>>> Tokens<F>
	for Lexer<F, E, C>
{
	type Error = Error<E>;

	fn peek(&mut self) -> Result<Loc<Option<&Token>, F>, Loc<Error<E>, F>> { self.peek() }

	fn next(&mut self) -> Result<Loc<Option<Token>, F>, Loc<Error<E>, F>> { self.next() }
}

fn is_pn_chars_base(c: char) -> bool {
	matches!(c, 'A'..='Z' | 'a'..='z' | '\u{00c0}'..='\u{00d6}' | '\u{00d8}'..='\u{00f6}' | '\u{00f8}'..='\u{02ff}' | '\u{0370}'..='\u{037d}' | '\u{037f}'..='\u{1fff}' | '\u{200c}'..='\u{200d}' | '\u{2070}'..='\u{218f}' | '\u{2c00}'..='\u{2fef}' | '\u{3001}'..='\u{d7ff}' | '\u{f900}'..='\u{fdcf}' | '\u{fdf0}'..='\u{fffd}' | '\u{10000}'..='\u{effff}')
}

fn is_pn_chars_u(c: char) -> bool { is_pn_chars_base(c) || c == '_' }

fn is_pn_chars(c: char) -> bool {
	is_pn_chars_u(c)
		|| matches!(c, '-' | '0'..='9' | '\u{00b7}' | '\u{0300}'..='\u{036f}' | '\u{203f}'..='\u{2040}')
}
