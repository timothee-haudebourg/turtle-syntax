use crate::{
	lexing::{Delimiter, Keyword, Punct, Token, Tokens},
	RdfLiteral
};
use locspan::{Loc, Location, MapLocErr, Span};
use std::fmt;

#[derive(Debug)]
pub enum Error<E> {
	Lexer(E),
	Unexpected(Option<Token>),
}

impl<E: fmt::Display> fmt::Display for Error<E> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Unexpected(None) => write!(f, "unexpected end of file"),
			Self::Unexpected(Some(token)) => write!(f, "unexpected {}", token),
			Self::Lexer(e) => e.fmt(f),
		}
	}
}

impl<E: 'static + std::error::Error> std::error::Error for Error<E> {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::Lexer(e) => Some(e),
			_ => None,
		}
	}
}

pub trait Parse<F>: Sized {
	#[allow(clippy::type_complexity)]
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(token), loc) => Self::parse_from(lexer, token, loc),
			Loc(None, loc) => Err(Loc(Error::Unexpected(None), loc)),
		}
	}

	#[allow(clippy::type_complexity)]
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>>;
}

impl<F: Clone> Parse<F> for crate::Document<F> {
	fn parse<L: Tokens<F>>(lexer: &mut L) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match lexer.next().map_loc_err(Error::Lexer)? {
			Loc(Some(token), token_loc) => Self::parse_from(lexer, token, token_loc),
			Loc(None, loc) => Ok(Loc(
				Self {
					statements: Vec::new(),
				},
				loc,
			)),
		}
	}

	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		token_loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		let mut statements = Vec::new();
		let statement = crate::Statement::parse_from(lexer, token, token_loc)?;
		let mut loc = statement.location().clone();
		statements.push(statement);

		while let Loc(Some(token), token_loc) = lexer.next().map_loc_err(Error::Lexer)? {
			let statement = crate::Statement::parse_from(lexer, token, token_loc)?;
			loc.span_mut().append(statement.span());
			statements.push(statement);
		}

		Ok(Loc(Self { statements }, loc))
	}
}

impl<F: Clone> Parse<F> for crate::Statement<F> {
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		mut loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::Keyword(Keyword::Prefix) => match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::Namespace(namespace)), ns_loc) => {
					match lexer.next().map_loc_err(Error::Lexer)? {
						Loc(Some(Token::IriRef(iri_ref)), iri_ref_loc) => {
							match lexer.next().map_loc_err(Error::Lexer)? {
								Loc(Some(Token::Punct(Punct::Period)), dot_loc) => {
									loc.span_mut().append(dot_loc.span());
									Ok(Loc(
										Self::Directive(crate::Directive::Prefix(
											Loc(namespace, ns_loc),
											Loc(iri_ref, iri_ref_loc),
										)),
										loc,
									))
								}
								Loc(unexpected, loc) => {
									Err(Loc(Error::Unexpected(unexpected), loc))
								}
							}
						}
						Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
					}
				}
				Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
			},
			Token::Keyword(Keyword::Base) => match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::IriRef(iri_ref)), iri_ref_loc) => {
					match lexer.next().map_loc_err(Error::Lexer)? {
						Loc(Some(Token::Punct(Punct::Period)), dot_loc) => {
							loc.span_mut().append(dot_loc.span());
							Ok(Loc(
								Self::Directive(crate::Directive::Base(Loc(iri_ref, iri_ref_loc))),
								loc,
							))
						}
						Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
					}
				}
				Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
			},
			Token::Keyword(Keyword::SparqlPrefix) => {
				match lexer.next().map_loc_err(Error::Lexer)? {
					Loc(Some(Token::Namespace(namespace)), ns_loc) => {
						match lexer.next().map_loc_err(Error::Lexer)? {
							Loc(Some(Token::IriRef(iri_ref)), iri_ref_loc) => {
								loc.span_mut().append(iri_ref_loc.span());
								Ok(Loc(
									Self::Directive(crate::Directive::SparqlPrefix(
										Loc(namespace, ns_loc),
										Loc(iri_ref, iri_ref_loc),
									)),
									loc,
								))
							}
							Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
						}
					}
					Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
				}
			}
			Token::Keyword(Keyword::SparqlBase) => match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::IriRef(iri_ref)), iri_ref_loc) => {
					loc.span_mut().append(iri_ref_loc.span());
					Ok(Loc(
						Self::Directive(crate::Directive::SparqlBase(Loc(
							iri_ref,
							iri_ref_loc,
						))),
						loc,
					))
				}
				Loc(unexpected, loc) => Err(Loc(Error::Unexpected(unexpected), loc)),
			},
			token => {
				let subject = crate::Subject::parse_from(lexer, token, loc)?;
				let mut loc = subject.location().clone();

				let po_list = match subject.value() {
					crate::Subject::BlankNode(crate::BlankNode::Anonymous(po_list))
						if !po_list.is_empty() =>
					{
						match lexer.next().map_loc_err(Error::Lexer)? {
							Loc(Some(Token::Punct(Punct::Period)), _) => Vec::new(),
							Loc(Some(token), token_loc) => {
								let Loc(po_list, po_list_loc) =
									parse_predicate_object_list_from(lexer, token, token_loc)?;
								loc.span_mut().append(po_list_loc.span());
								match lexer.next().map_loc_err(Error::Lexer)? {
									Loc(Some(Token::Punct(Punct::Period)), _) => po_list,
									Loc(unexpected, loc) => {
										return Err(Loc(Error::Unexpected(unexpected), loc))
									}
								}
							}
							Loc(unexpected, loc) => {
								return Err(Loc(Error::Unexpected(unexpected), loc))
							}
						}
					}
					_ => {
						let Loc(po_list, po_list_loc) = parse_predicate_object_list(lexer)?;
						loc.span_mut().append(po_list_loc.span());
						match lexer.next().map_loc_err(Error::Lexer)? {
							Loc(Some(Token::Punct(Punct::Period)), _) => po_list,
							Loc(unexpected, loc) => {
								return Err(Loc(Error::Unexpected(unexpected), loc))
							}
						}
					}
				};

				Ok(Loc(crate::Statement::Triples(subject, po_list), loc))
			}
		}
	}
}

fn compact_iri<F: Clone>(
	loc: &Location<F>,
	prefix: Option<(String, Span)>,
	(suffix, suffix_span): (String, Span),
) -> crate::Iri<F> {
	crate::Iri::Compact(
		prefix.map(|(prefix, prefix_span)| {
			Loc(prefix, Location::new(loc.file().clone(), prefix_span))
		}),
		Loc(suffix, Location::new(loc.file().clone(), suffix_span)),
	)
}

impl<F: Clone> Parse<F> for crate::Subject<F> {
	#[allow(clippy::type_complexity)]
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: locspan::Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::IriRef(iri_ref) => {
				Ok(Loc(crate::Subject::Iri(crate::Iri::IriRef(iri_ref)), loc))
			}
			Token::CompactIri(prefix, suffix) => Ok(Loc(
				crate::Subject::Iri(compact_iri(&loc, prefix, suffix)),
				loc,
			)),
			Token::BlankNodeLabel(label) => Ok(Loc(
				crate::Subject::BlankNode(crate::BlankNode::Label(label)),
				loc,
			)),
			Token::Begin(Delimiter::Bracket) => {
				let Loc(po_list, loc) = parse_blank_node_property_list(lexer, loc)?;
				Ok(Loc(
					crate::Subject::BlankNode(crate::BlankNode::Anonymous(po_list)),
					loc,
				))
			}
			Token::Begin(Delimiter::Parenthesis) => {
				let mut objects = Vec::new();

				let loc = loop {
					match lexer.next().map_loc_err(Error::Lexer)? {
						Loc(Some(Token::End(Delimiter::Parenthesis)), end_loc) => {
							break loc.with(end_loc.span())
						}
						Loc(Some(token), loc) => {
							let object = crate::Object::parse_from(lexer, token, loc)?;
							objects.push(object)
						}
						Loc(unexpected, loc) => {
							return Err(Loc(Error::Unexpected(unexpected), loc))
						}
					}
				};

				Ok(Loc(crate::Subject::Collection(objects), loc))
			}
			unexpected => Err(Loc(Error::Unexpected(Some(unexpected)), loc)),
		}
	}
}

#[allow(clippy::type_complexity)]
fn parse_predicate_object_list_from<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	token: Token,
	token_loc: Location<F>,
) -> Result<Loc<Vec<Loc<crate::PredicateObjects<F>, F>>, F>, Loc<Error<L::Error>, F>> {
	let mut predicate_objects = Vec::new();
	let po = crate::PredicateObjects::parse_from(lexer, token, token_loc)?;
	let loc = po.location().clone();
	predicate_objects.push(po);

	parse_predicate_object_rest(lexer, predicate_objects, loc)
}

#[allow(clippy::type_complexity)]
fn parse_predicate_object_list<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
) -> Result<Loc<Vec<Loc<crate::PredicateObjects<F>, F>>, F>, Loc<Error<L::Error>, F>> {
	let mut predicate_objects = Vec::new();
	let po = crate::PredicateObjects::parse(lexer)?;
	let loc = po.location().clone();
	predicate_objects.push(po);

	parse_predicate_object_rest(lexer, predicate_objects, loc)
}

#[allow(clippy::type_complexity)]
fn parse_predicate_object_rest<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	mut predicate_objects: Vec<Loc<crate::PredicateObjects<F>, F>>,
	mut loc: Location<F>,
) -> Result<Loc<Vec<Loc<crate::PredicateObjects<F>, F>>, F>, Loc<Error<L::Error>, F>> {
	while let Loc(Some(Token::Punct(Punct::Semicolon)), _) =
		lexer.peek().map_loc_err(Error::Lexer)?
	{
		lexer.next().map_loc_err(Error::Lexer)?;
		let po = crate::PredicateObjects::parse(lexer)?;
		loc.span_mut().append(po.span());
		predicate_objects.push(po);
	}

	Ok(Loc(predicate_objects, loc))
}

#[allow(clippy::type_complexity)]
fn parse_blank_node_property_list<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	bracket_loc: Location<F>,
) -> Result<Loc<Vec<Loc<crate::PredicateObjects<F>, F>>, F>, Loc<Error<L::Error>, F>> {
	let mut predicate_objects = Vec::new();
	let loc = match lexer.next().map_loc_err(Error::Lexer)? {
		Loc(Some(Token::End(Delimiter::Bracket)), end_loc) => bracket_loc.with(end_loc.span()),
		Loc(Some(token), loc) => {
			predicate_objects.push(crate::PredicateObjects::parse_from(lexer, token, loc)?);
			loop {
				match lexer.next().map_loc_err(Error::Lexer)? {
					Loc(Some(Token::End(Delimiter::Bracket)), end_loc) => {
						break bracket_loc.with(end_loc.span())
					}
					Loc(Some(Token::Punct(Punct::Semicolon)), _) => {
						predicate_objects.push(crate::PredicateObjects::parse(lexer)?);
					}
					Loc(unexpected, loc) => return Err(Loc(Error::Unexpected(unexpected), loc)),
				}
			}
		}
		Loc(None, loc) => return Err(Loc(Error::Unexpected(None), loc)),
	};

	Ok(Loc(predicate_objects, loc))
}

impl<F: Clone> Parse<F> for crate::Object<F> {
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::IriRef(iri_ref) => Ok(Loc(crate::Object::Iri(crate::Iri::IriRef(iri_ref)), loc)),
			Token::CompactIri(prefix, suffix) => Ok(Loc(
				crate::Object::Iri(compact_iri(&loc, prefix, suffix)),
				loc,
			)),
			Token::BlankNodeLabel(label) => Ok(Loc(
				crate::Object::BlankNode(crate::BlankNode::Label(label)),
				loc,
			)),
			Token::Begin(Delimiter::Bracket) => {
				let Loc(po_list, loc) = parse_blank_node_property_list(lexer, loc)?;
				Ok(Loc(
					crate::Object::BlankNode(crate::BlankNode::Anonymous(po_list)),
					loc,
				))
			}
			Token::Begin(Delimiter::Parenthesis) => {
				// collection
				let mut objects = Vec::new();

				let loc = loop {
					match lexer.next().map_loc_err(Error::Lexer)? {
						Loc(Some(Token::End(Delimiter::Parenthesis)), end_loc) => {
							break loc.with(end_loc.span())
						}
						Loc(Some(token), loc) => {
							let object = Self::parse_from(lexer, token, loc)?;
							objects.push(object)
						}
						Loc(unexpected, loc) => {
							return Err(Loc(Error::Unexpected(unexpected), loc))
						}
					}
				};

				Ok(Loc(crate::Object::Collection(objects), loc))
			}
			token => {
				let Loc(lit, loc) = crate::Literal::parse_from(lexer, token, loc)?;
				Ok(Loc(crate::Object::Literal(lit), loc))
			}
		}
	}
}

impl<F: Clone> Parse<F> for crate::PredicateObjects<F> {
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		let verb = crate::Verb::parse_from(lexer, token, loc)?;
		let objects = parse_object_list(lexer)?;
		let loc = verb.location().clone().with(objects.span());

		Ok(Loc(crate::PredicateObjects { verb, objects }, loc))
	}
}

impl<F: Clone> Parse<F> for crate::Verb<F> {
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::Keyword(Keyword::A) => Ok(Loc(crate::Verb::A, loc)),
			token => {
				let Loc(iri, loc) = crate::Iri::parse_from(lexer, token, loc)?;
				Ok(Loc(crate::Verb::Predicate(iri), loc))
			}
		}
	}
}

impl<F: Clone> Parse<F> for crate::Iri<F> {
	fn parse_from<L: Tokens<F>>(
		_lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::IriRef(iri_ref) => Ok(Loc(
				crate::Iri::IriRef(iri_ref),
				loc,
			)),
			Token::CompactIri(prefix, (suffix, suffix_span)) => Ok(Loc(
				crate::Iri::Compact(
					prefix.map(|(prefix, prefix_span)| {
						Loc(prefix, Location::new(loc.file().clone(), prefix_span))
					}),
					Loc(suffix, Location::new(loc.file().clone(), suffix_span)),
				),
				loc,
			)),
			unexpected => Err(Loc(Error::Unexpected(Some(unexpected)), loc)),
		}
	}
}

/// Parses a non empty comma separated list of objects.
#[allow(clippy::type_complexity)]
fn parse_object_list<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
) -> Result<Loc<Vec<Loc<crate::Object<F>, F>>, F>, Loc<Error<L::Error>, F>> {
	let mut objects = Vec::new();

	let object = crate::Object::parse(lexer)?;
	let mut loc = object.location().clone();
	objects.push(object);

	while let Loc(Some(Token::Punct(Punct::Comma)), _) = lexer.peek().map_loc_err(Error::Lexer)? {
		lexer.next().map_loc_err(Error::Lexer)?;
		let object = crate::Object::parse(lexer)?;
		loc.span_mut().append(object.span());
		objects.push(object);
	}

	Ok(Loc(objects, loc))
}

#[allow(clippy::type_complexity)]
fn parse_rdf_literal<F: Clone, L: Tokens<F>>(
	lexer: &mut L,
	string: rdf_types::StringLiteral,
	string_loc: locspan::Location<F>,
) -> Result<Loc<RdfLiteral<F>, F>, Loc<Error<L::Error>, F>> {
	match lexer.peek().map_loc_err(Error::Lexer)? {
		Loc(Some(Token::LangTag(_)), tag_loc) => {
			let tag = match lexer.next().map_loc_err(Error::Lexer)? {
				Loc(Some(Token::LangTag(tag)), _) => tag,
				_ => panic!("expected lang tag"),
			};

			let mut loc = string_loc.clone();
			loc.span_mut().append(tag_loc.span());
			Ok(Loc(
				RdfLiteral::LangString(Loc(string, string_loc), Loc(tag, tag_loc)),
				loc,
			))
		}
		Loc(Some(Token::Punct(Punct::Carets)), _) => {
			lexer.next().map_loc_err(Error::Lexer)?;
			let iri = crate::Iri::parse(lexer)?;
			let loc = string_loc.clone().with(iri.span());
			Ok(Loc(
				RdfLiteral::TypedString(
					Loc(string, string_loc),
					iri
				),
				loc
			))
		}
		_ => Ok(Loc(
			RdfLiteral::String(Loc(string, string_loc.clone())),
			string_loc,
		)),
	}
}

impl<F: Clone> Parse<F> for crate::Literal<F> {
	fn parse_from<L: Tokens<F>>(
		lexer: &mut L,
		token: Token,
		loc: Location<F>,
	) -> Result<Loc<Self, F>, Loc<Error<L::Error>, F>> {
		match token {
			Token::StringLiteral(string) => {
				let Loc(lit, loc) = parse_rdf_literal(lexer, string, loc)?;
				Ok(Loc(crate::Literal::Rdf(lit), loc))
			}
			Token::Number(n) => Ok(Loc(crate::Literal::Number(n), loc)),
			Token::Keyword(Keyword::True) => Ok(Loc(crate::Literal::Boolean(true), loc)),
			Token::Keyword(Keyword::False) => Ok(Loc(crate::Literal::Boolean(false), loc)),
			unexpected => Err(Loc(Error::Unexpected(Some(unexpected)), loc)),
		}
	}
}
