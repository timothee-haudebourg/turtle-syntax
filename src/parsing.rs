use crate::{
	lexing::{self, Delimiter, Keyword, Punct, Token, Tokens},
	Collection, Lexer, RdfLiteral,
};
use decoded_char::DecodedChar;
use locspan::{Meta, Span};

/// Unexpected char or end of file.
#[derive(Debug, thiserror::Error)]
pub enum Unexpected {
	#[error("unexpected token `{0}`")]
	Token(Token),

	#[error("unexpected end of file")]
	EndOfFile,
}

impl From<Option<Token>> for Unexpected {
	fn from(value: Option<Token>) -> Self {
		match value {
			Some(token) => Unexpected::Token(token),
			None => Unexpected::EndOfFile,
		}
	}
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
	#[error(transparent)]
	Lexer(E),

	#[error(transparent)]
	Unexpected(Unexpected),
}

pub type MetaError<E, M> = Meta<Box<Error<E>>, M>;

pub trait Parse<M>: Sized {
	#[allow(clippy::type_complexity)]
	fn parse_with<L, F>(parser: &mut Parser<L, F>) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match parser.next()? {
			Meta(Some(token), span) => Self::parse_from(parser, Meta(token, span)),
			Meta(None, span) => Self::parse_empty::<L>(parser.build_metadata(span)),
		}
	}

	#[allow(clippy::type_complexity)]
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		token: Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M;

	#[allow(clippy::type_complexity)]
	fn parse_empty<L>(meta: M) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
	{
		Err(Meta(
			Box::new(Error::Unexpected(Unexpected::EndOfFile)),
			meta,
		))
	}

	#[inline(always)]
	fn parse<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<DecodedChar, E>>,
		F: FnMut(Span) -> M,
	{
		let mut parser = Parser::new(Lexer::new(chars), metadata_builder);
		Self::parse_with(&mut parser)
	}

	#[inline(always)]
	fn parse_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = DecodedChar>,
		F: FnMut(Span) -> M,
	{
		Self::parse(chars.map(Ok), metadata_builder)
	}

	#[inline(always)]
	fn parse_utf8<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<char, E>>,
		F: FnMut(Span) -> M,
	{
		Self::parse(
			decoded_char::FallibleUtf8Decoded::new(chars),
			metadata_builder,
		)
	}

	#[inline(always)]
	fn parse_utf8_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = char>,
		F: FnMut(Span) -> M,
	{
		Self::parse_infallible(decoded_char::Utf8Decoded::new(chars), metadata_builder)
	}

	#[inline(always)]
	fn parse_utf16<C, F, E>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error<E>, M>>
	where
		C: Iterator<Item = Result<char, E>>,
		F: FnMut(Span) -> M,
	{
		Self::parse(
			decoded_char::FallibleUtf16Decoded::new(chars),
			metadata_builder,
		)
	}

	#[inline(always)]
	fn parse_utf16_infallible<C, F>(
		chars: C,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		C: Iterator<Item = char>,
		F: FnMut(Span) -> M,
	{
		Self::parse_infallible(decoded_char::Utf16Decoded::new(chars), metadata_builder)
	}

	#[inline(always)]
	fn parse_str<F>(
		string: &str,
		metadata_builder: F,
	) -> Result<Meta<Self, M>, MetaError<lexing::Error, M>>
	where
		F: FnMut(Span) -> M,
	{
		Self::parse_utf8_infallible(string.chars(), metadata_builder)
	}
}

pub struct Parser<L, F> {
	lexer: L,
	metadata_builder: F,
}

impl<L, F> Parser<L, F> {
	pub fn new(lexer: L, metadata_builder: F) -> Self {
		Self {
			lexer,
			metadata_builder,
		}
	}
}

impl<L: Tokens, F: FnMut(Span) -> M, M> Parser<L, F> {
	fn next(&mut self) -> Result<Meta<Option<Token>, Span>, MetaError<L::Error, M>> {
		self.lexer
			.next()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), (self.metadata_builder)(span)))
	}

	#[allow(clippy::type_complexity)]
	fn peek(&mut self) -> Result<Meta<Option<&Token>, Span>, MetaError<L::Error, M>> {
		self.lexer
			.peek()
			.map_err(|Meta(e, span)| Meta(Box::new(Error::Lexer(e)), (self.metadata_builder)(span)))
	}

	fn last_span(&self) -> Span {
		self.lexer.last()
	}

	fn build_metadata(&mut self, span: Span) -> M {
		(self.metadata_builder)(span)
	}
}

impl<M> Parse<M> for crate::Document<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut result = crate::Document::default();
		result.insert(crate::Statement::parse_from(parser, Meta(token, span))?);

		while let Meta(Some(token), span) = parser.next()? {
			result.insert(crate::Statement::parse_from(parser, Meta(token, span))?)
		}

		span.append(parser.last_span());
		Ok(Meta(result, parser.build_metadata(span)))
	}

	fn parse_empty<L>(meta: M) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
	{
		Ok(Meta(Self::new(), meta))
	}
}

impl<M> Parse<M> for crate::Directive<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::Keyword(Keyword::Prefix) => match parser.next()? {
				Meta(Some(Token::Namespace(namespace)), ns_span) => match parser.next()? {
					Meta(Some(Token::IriRef(iri_ref)), iri_ref_span) => match parser.next()? {
						Meta(Some(Token::Punct(Punct::Period)), dot_span) => {
							span.append(dot_span);
							Ok(Meta(
								crate::Directive::Prefix(
									Meta(namespace, parser.build_metadata(ns_span)),
									Meta(iri_ref, parser.build_metadata(iri_ref_span)),
								),
								parser.build_metadata(span),
							))
						}
						Meta(unexpected, span) => Err(Meta(
							Box::new(Error::Unexpected(unexpected.into())),
							parser.build_metadata(span),
						)),
					},
					Meta(unexpected, span) => Err(Meta(
						Box::new(Error::Unexpected(unexpected.into())),
						parser.build_metadata(span),
					)),
				},
				Meta(unexpected, span) => Err(Meta(
					Box::new(Error::Unexpected(unexpected.into())),
					parser.build_metadata(span),
				)),
			},
			Token::Keyword(Keyword::Base) => match parser.next()? {
				Meta(Some(Token::IriRef(iri_ref)), iri_ref_span) => match parser.next()? {
					Meta(Some(Token::Punct(Punct::Period)), dot_span) => {
						span.append(dot_span);
						Ok(Meta(
							crate::Directive::Base(Meta(
								iri_ref,
								parser.build_metadata(iri_ref_span),
							)),
							parser.build_metadata(span),
						))
					}
					Meta(unexpected, span) => Err(Meta(
						Box::new(Error::Unexpected(unexpected.into())),
						parser.build_metadata(span),
					)),
				},
				Meta(unexpected, span) => Err(Meta(
					Box::new(Error::Unexpected(unexpected.into())),
					parser.build_metadata(span),
				)),
			},
			Token::Keyword(Keyword::SparqlPrefix) => match parser.next()? {
				Meta(Some(Token::Namespace(namespace)), ns_span) => match parser.next()? {
					Meta(Some(Token::IriRef(iri_ref)), iri_ref_span) => {
						span.append(iri_ref_span);
						Ok(Meta(
							crate::Directive::SparqlPrefix(
								Meta(namespace, parser.build_metadata(ns_span)),
								Meta(iri_ref, parser.build_metadata(iri_ref_span)),
							),
							parser.build_metadata(span),
						))
					}
					Meta(unexpected, span) => Err(Meta(
						Box::new(Error::Unexpected(unexpected.into())),
						parser.build_metadata(span),
					)),
				},
				Meta(unexpected, span) => Err(Meta(
					Box::new(Error::Unexpected(unexpected.into())),
					parser.build_metadata(span),
				)),
			},
			Token::Keyword(Keyword::SparqlBase) => match parser.next()? {
				Meta(Some(Token::IriRef(iri_ref)), iri_ref_span) => {
					span.append(iri_ref_span);
					Ok(Meta(
						crate::Directive::SparqlBase(Meta(
							iri_ref,
							parser.build_metadata(iri_ref_span),
						)),
						parser.build_metadata(span),
					))
				}
				Meta(unexpected, span) => Err(Meta(
					Box::new(Error::Unexpected(unexpected.into())),
					parser.build_metadata(span),
				)),
			},
			unexpected => Err(Meta(
				Box::new(Error::Unexpected(Unexpected::Token(unexpected))),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M> Parse<M> for crate::Statement<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			token @ Token::Keyword(
				Keyword::Prefix | Keyword::Base | Keyword::SparqlPrefix | Keyword::SparqlBase,
			) => Ok(crate::Directive::parse_from(parser, Meta(token, span))?.map(Self::Directive)),
			token => {
				let Meta(triples, meta) = crate::Triples::parse_from(parser, Meta(token, span))?;
				Ok(Meta(crate::Statement::Triples(triples), meta))
			}
		}
	}
}

impl<M> Parse<M> for crate::Triples<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let subject = crate::Subject::parse_from(parser, Meta(token, span))?;

		let po_list = match parser.peek()? {
			Meta(Some(Token::Punct(Punct::Period)), p_span) => {
				if !matches!(&subject, Meta(crate::Subject::BlankNode(crate::BlankNode::Anonymous(l)), _) if !l.is_empty())
				{
					return Err(Meta(
						Box::new(Error::Unexpected(Unexpected::Token(Token::Punct(
							Punct::Period,
						)))),
						parser.build_metadata(p_span),
					));
				}

				let span = parser.last_span().next();
				Meta(Vec::new(), parser.build_metadata(span))
			}
			_ => Vec::parse_with(parser)?,
		};

		span.append(parser.last_span());

		match parser.next()? {
			Meta(Some(Token::Punct(Punct::Period)), _) => (),
			Meta(unexpected, span) => {
				return Err(Meta(
					Box::new(Error::Unexpected(unexpected.into())),
					parser.build_metadata(span),
				));
			}
		}

		Ok(Meta(
			crate::Triples {
				subject,
				predicate_objects_list: po_list,
			},
			parser.build_metadata(span),
		))
	}
}

impl<M> Parse<M> for Vec<Meta<crate::PredicateObjects<M>, M>> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut result = vec![crate::PredicateObjects::parse_from(
			parser,
			Meta(token, span),
		)?];

		loop {
			match parser.peek()? {
				Meta(Some(Token::Punct(Punct::Semicolon)), _) => {
					parser.next()?;
					result.push(crate::PredicateObjects::parse_with(parser)?);
				}
				Meta(Some(Token::Punct(Punct::Period) | Token::End(Delimiter::Bracket)), _) => {
					break
				}
				_ => {
					let Meta(unexpected, span) = parser.next()?;
					return Err(Meta(
						Box::new(Error::Unexpected(unexpected.into())),
						parser.build_metadata(span),
					));
				}
			}
		}

		Ok(Meta(result, parser.build_metadata(span)))
	}
}

impl<M> Parse<M> for crate::PredicateObjects<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let verb = crate::Verb::parse_from(parser, Meta(token, span))?;
		let objects = crate::Objects::parse_with(parser)?;
		span.append(parser.last_span());
		Ok(Meta(Self { verb, objects }, parser.build_metadata(span)))
	}
}

impl<M> Parse<M> for crate::Objects<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		let mut result = vec![crate::Object::parse_from(parser, Meta(token, span))?];

		loop {
			match parser.peek()? {
				Meta(Some(Token::Punct(Punct::Comma)), _) => {
					parser.next()?;
					result.push(crate::Object::parse_with(parser)?);
				}
				Meta(
					Some(
						Token::Punct(Punct::Period | Punct::Semicolon)
						| Token::End(Delimiter::Bracket),
					),
					_,
				) => break,
				_ => {
					let Meta(unexpected, span) = parser.next()?;
					return Err(Meta(
						Box::new(Error::Unexpected(unexpected.into())),
						parser.build_metadata(span),
					));
				}
			}
		}

		Ok(Meta(Self(result), parser.build_metadata(span)))
	}
}

fn compact_iri<M, L, F>(
	parser: &mut Parser<L, F>,
	(prefix, prefix_span): (String, Span),
	(suffix, suffix_span): (String, Span),
) -> crate::Iri<M>
where
	L: Tokens,
	F: FnMut(Span) -> M,
{
	crate::Iri::Compact(
		Meta(prefix, parser.build_metadata(prefix_span)),
		Meta(suffix, parser.build_metadata(suffix_span)),
	)
}

impl<M> Parse<M> for crate::Subject<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::IriRef(iri_ref) => Ok(Meta(
				crate::Subject::Iri(crate::Iri::IriRef(iri_ref)),
				parser.build_metadata(span),
			)),
			Token::CompactIri(prefix, suffix) => Ok(Meta(
				crate::Subject::Iri(compact_iri(parser, prefix, suffix)),
				parser.build_metadata(span),
			)),
			Token::BlankNodeLabel(label) => Ok(Meta(
				crate::Subject::BlankNode(crate::BlankNode::Label(label)),
				parser.build_metadata(span),
			)),
			Token::Begin(Delimiter::Bracket) => {
				let po_list = match parser.peek()? {
					Meta(Some(Token::End(Delimiter::Bracket)), _) => {
						let span = parser.last_span().next();
						Meta(Vec::new(), parser.build_metadata(span))
					}
					_ => Vec::parse_with(parser)?,
				};

				match parser.next()? {
					Meta(Some(Token::End(Delimiter::Bracket)), _) => (),
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected.into())),
							parser.build_metadata(span),
						));
					}
				}

				span.append(parser.last_span());
				Ok(Meta(
					crate::Subject::BlankNode(crate::BlankNode::Anonymous(po_list)),
					parser.build_metadata(span),
				))
			}
			Token::Begin(Delimiter::Parenthesis) => {
				let Meta(objects, meta) = Collection::parse_from(parser, Meta(token, span))?;
				Ok(Meta(crate::Subject::Collection(objects), meta))
			}
			unexpected => Err(Meta(
				Box::new(Error::Unexpected(Unexpected::Token(unexpected))),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M> Parse<M> for Collection<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::Begin(Delimiter::Parenthesis) => {
				let mut objects = Vec::new();

				loop {
					match parser.next()? {
						Meta(Some(Token::End(Delimiter::Parenthesis)), end_span) => {
							span.append(end_span);
							break;
						}
						Meta(Some(token), span) => {
							let object = crate::Object::parse_from(parser, Meta(token, span))?;
							objects.push(object)
						}
						Meta(unexpected, span) => {
							return Err(Meta(
								Box::new(Error::Unexpected(unexpected.into())),
								parser.build_metadata(span),
							))
						}
					}
				}

				Ok(Meta(Collection(objects), parser.build_metadata(span)))
			}
			unexpected => Err(Meta(
				Box::new(Error::Unexpected(Unexpected::Token(unexpected))),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M> Parse<M> for crate::Object<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, mut span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::IriRef(iri_ref) => Ok(Meta(
				crate::Object::Iri(crate::Iri::IriRef(iri_ref)),
				parser.build_metadata(span),
			)),
			Token::CompactIri(prefix, suffix) => Ok(Meta(
				crate::Object::Iri(compact_iri(parser, prefix, suffix)),
				parser.build_metadata(span),
			)),
			Token::BlankNodeLabel(label) => Ok(Meta(
				crate::Object::BlankNode(crate::BlankNode::Label(label)),
				parser.build_metadata(span),
			)),
			Token::Begin(Delimiter::Bracket) => {
				let po_list = match parser.peek()? {
					Meta(Some(Token::End(Delimiter::Bracket)), _) => {
						let span = parser.last_span().next();
						Meta(Vec::new(), parser.build_metadata(span))
					}
					_ => Vec::parse_with(parser)?,
				};

				match parser.next()? {
					Meta(Some(Token::End(Delimiter::Bracket)), _) => (),
					Meta(unexpected, span) => {
						return Err(Meta(
							Box::new(Error::Unexpected(unexpected.into())),
							parser.build_metadata(span),
						));
					}
				}

				span.append(parser.last_span());
				Ok(Meta(
					crate::Object::BlankNode(crate::BlankNode::Anonymous(po_list)),
					parser.build_metadata(span),
				))
			}
			Token::Begin(Delimiter::Parenthesis) => {
				let Meta(objects, meta) = Collection::parse_from(parser, Meta(token, span))?;
				Ok(Meta(crate::Object::Collection(objects), meta))
			}
			token => {
				let Meta(literal, meta) = crate::Literal::parse_from(parser, Meta(token, span))?;
				Ok(Meta(crate::Object::Literal(literal), meta))
			}
		}
	}
}

#[allow(clippy::type_complexity)]
fn parse_rdf_literal<M, L, F>(
	parser: &mut Parser<L, F>,
	Meta(string, string_span): Meta<String, Span>,
) -> Result<Meta<RdfLiteral<M>, M>, MetaError<L::Error, M>>
where
	L: Tokens,
	F: FnMut(Span) -> M,
{
	match parser.peek()? {
		Meta(Some(Token::LangTag(_)), tag_span) => {
			let tag = match parser.next()? {
				Meta(Some(Token::LangTag(tag)), _) => tag,
				_ => panic!("expected lang tag"),
			};

			let span = string_span.union(tag_span);
			Ok(Meta(
				RdfLiteral::LangString(
					Meta(string, parser.build_metadata(string_span)),
					Meta(tag, parser.build_metadata(tag_span)),
				),
				parser.build_metadata(span),
			))
		}
		Meta(Some(Token::Punct(Punct::Carets)), _) => {
			parser.next()?;
			let iri = crate::Iri::parse_with(parser)?;
			let span = string_span.union(parser.last_span());
			Ok(Meta(
				RdfLiteral::TypedString(Meta(string, parser.build_metadata(string_span)), iri),
				parser.build_metadata(span),
			))
		}
		_ => Ok(Meta(
			RdfLiteral::String(Meta(string, parser.build_metadata(string_span))),
			parser.build_metadata(string_span),
		)),
	}
}

impl<M> Parse<M> for crate::Literal<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::StringLiteral(string) => {
				let Meta(lit, loc) = parse_rdf_literal(parser, Meta(string, span))?;
				Ok(Meta(crate::Literal::Rdf(lit), loc))
			}
			Token::Numeric(n) => Ok(Meta(
				crate::Literal::Numeric(n),
				parser.build_metadata(span),
			)),
			Token::Keyword(Keyword::True) => Ok(Meta(
				crate::Literal::Boolean(true),
				parser.build_metadata(span),
			)),
			Token::Keyword(Keyword::False) => Ok(Meta(
				crate::Literal::Boolean(false),
				parser.build_metadata(span),
			)),
			unexpected => Err(Meta(
				Box::new(Error::Unexpected(Unexpected::Token(unexpected))),
				parser.build_metadata(span),
			)),
		}
	}
}

impl<M> Parse<M> for crate::Verb<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::Keyword(Keyword::A) => Ok(Meta(crate::Verb::A, parser.build_metadata(span))),
			token => {
				let Meta(iri, meta) = crate::Iri::parse_from(parser, Meta(token, span))?;
				Ok(Meta(crate::Verb::Predicate(iri), meta))
			}
		}
	}
}

impl<M> Parse<M> for crate::Iri<M> {
	fn parse_from<L, F>(
		parser: &mut Parser<L, F>,
		Meta(token, span): Meta<Token, Span>,
	) -> Result<Meta<Self, M>, MetaError<L::Error, M>>
	where
		L: Tokens,
		F: FnMut(Span) -> M,
	{
		match token {
			Token::IriRef(iri_ref) => Ok(Meta(
				crate::Iri::IriRef(iri_ref),
				parser.build_metadata(span),
			)),
			Token::CompactIri(prefix, suffix) => Ok(Meta(
				compact_iri(parser, prefix, suffix),
				parser.build_metadata(span),
			)),
			unexpected => Err(Meta(
				Box::new(Error::Unexpected(Unexpected::Token(unexpected))),
				parser.build_metadata(span),
			)),
		}
	}
}
