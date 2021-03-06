pub mod error;
mod ast;
pub mod lexer;

use std::iter::Peekable;
use source_span::{Span, Position};
use crate::Loc;
use error::*;
pub use ast::*;
use lexer::{Token, Keyword, Delimiter};
pub use lexer::Lexer;

pub trait Parsable: Sized {
    fn parse<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Self>>;
}

fn peek<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>) -> Result<Option<Loc<Token>>> {
    match lexer.peek() {
        Some(Ok(token)) => Ok(Some(token.clone())),
        Some(Err(_)) => {
            let mut dummy_span = Span::default();
            consume(lexer, &mut dummy_span)
        },
        None => Ok(None)
    }
}

fn peek_token<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Token>> {
    match peek(lexer) {
        Ok(Some(token)) => Ok(token),
        Ok(None) => Err(Loc::new(Error::UnexpectedEos, pos.into())),
        Err(e) => Err(e)
    }
}

fn consume<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Option<Loc<Token>>> {
    match lexer.next() {
        Some(Ok(token)) => {
            // trace!("token: {:?}", token);
            if span.is_empty() {
                *span = token.span();
            } else {
                span.append(token.span());
            }
            Ok(Some(token.clone()))
        },
        Some(Err(e)) => Err(e.into()),
        None => Ok(None)
    }
}

fn expect<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<Loc<Token>> {
    match consume(lexer, span) {
        Ok(Some(token)) => Ok(token),
        Ok(None) => Err(Loc::new(Error::UnexpectedEos, span.end().into())),
        Err(e) => Err(e)
    }
}

impl Parsable for Document {
    fn parse<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Self>> {
        let mut span: Span = pos.into();
        let mut statements = Vec::new();
        while peek(lexer)?.is_some() {
            let stm = Statement::parse(lexer, span.end())?;
            span.append(stm.span());
            statements.push(stm);
        }

        Ok(Loc::new(Document {
            statements
        }, span))
    }
}

pub fn expect_dot<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, span: &mut Span) -> Result<()> {
    let (token, dot_span) = expect(lexer, span)?.into_raw_parts();
    match token {
        Token::Punct('.') => {
            Ok(())
        },
        unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), dot_span))
    }
}

impl Parsable for Statement {
    fn parse<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Self>> {
        let mut span: Span = pos.into();
        let (token, _) = peek_token(lexer, span.end())?.into_raw_parts();
        let stm = match token {
            Token::Keyword(Keyword::Prefix) => {
                consume(lexer, &mut span)?;
                let (token, id_span) = expect(lexer, &mut span)?.into_raw_parts();
                match token {
                    Token::Ident(id) => {
                        let (token, iri_span) = expect(lexer, &mut span)?.into_raw_parts();
                        match token {
                            Token::Iri(iri) => {
                                let (token, dot_span) = expect(lexer, &mut span)?.into_raw_parts();
                                match token {
                                    Token::Punct('.') => {
                                        Statement::Directive(Directive::Prefix(
                                            Loc::new(id, id_span),
                                            Loc::new(iri, iri_span)
                                        ))
                                    },
                                    unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), dot_span))
                                }
                            },
                            unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), iri_span))
                        }
                    },
                    unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), id_span))
                }
            },
            Token::Keyword(Keyword::Base) => {
                consume(lexer, &mut span)?;
                let (token, iri_span) = expect(lexer, &mut span)?.into_raw_parts();
                match token {
                    Token::Iri(iri) => {
                        expect_dot(lexer, &mut span)?;
                        Statement::Directive(Directive::Base(
                            Loc::new(iri, iri_span)
                        ))
                    },
                    unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), iri_span))
                }
            },
            _ => {
                let subject = parse_subject(lexer, span.end())?;
                let verb_objects_list = parse_verb_objects_list(lexer, span.end())?;
                span.append(verb_objects_list.span());
                expect_dot(lexer, &mut span)?;
                Statement::Triples(subject, verb_objects_list)
            }
        };

        Ok(Loc::new(stm, span))
    }
}

fn parse_verb_objects_list<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Vec<Loc<VerbObjects>>>> {
    let mut span: Span = pos.into();
    let mut list = Vec::new();
    loop {
        if let Some(token) = peek(lexer)? {
            let (token, verb_span) = token.into_raw_parts();
            match token {
                Token::Punct('.') => break,
                Token::Punct(';') => {
                    consume(lexer, &mut span)?;
                },
                _ => {
                    consume(lexer, &mut span)?;
                    let verb = Loc::new(match token {
                        Token::Ident(id) => {
                            Verb::from(id.as_str())
                        },
                        Token::Iri(iri) => {
                            Verb::Predicate(IriRef::Iri(iri))
                        },
                        unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), verb_span))
                    }, verb_span);
                    let objects = parse_objects(lexer, span.end())?;
                    let verb_objects_span = verb_span.union(objects.span());
                    span.append(verb_objects_span);
                    list.push(Loc::new(VerbObjects {
                        verb: verb,
                        objects: objects
                    }, verb_objects_span))
                }
            }
        } else {
            break
        }
    }
    Ok(Loc::new(list, span))
}

fn safe_token(token: Loc<Token>) -> lexer::Result<Loc<Token>> {
    Ok(token)
}

fn parse_subject<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Subject>> {
    let mut span: Span = pos.into();
    let (token, token_span) = expect(lexer, &mut span)?.into_raw_parts();
    let subject = match token {
        Token::Ident(id) => {
            Subject::from(id.as_str())
        },
        Token::Iri(iri) => {
            Subject::Iri(IriRef::Iri(iri))
        },
        Token::Group(Delimiter::Bracket, group) => {
            let mut lexer = group.into_iter().map(safe_token).peekable();
            let (inner_list, subject_span) = parse_verb_objects_list(&mut lexer, span.end())?.into_raw_parts();
            span.append(subject_span);
            Subject::Anonymous(inner_list)
        },
        Token::Group(Delimiter::Parenthesis, group) => {
            let mut subjects = Vec::new();
            let mut lexer = group.into_iter().map(safe_token).peekable();
            while let Some(_) = lexer.peek() {
                let subject = parse_subject(&mut lexer, span.end())?;
                span.append(subject.span());
                subjects.push(subject);
            }
            Subject::Collection(subjects)
        },
        unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
    };

    Ok(Loc::new(subject, span))
}

fn parse_object<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Object>> {
    let mut span: Span = pos.into();
    let (token, token_span) = expect(lexer, &mut span)?.into_raw_parts();
    let object = match token {
        Token::Numeric(n) => {
            Object::Literal(Literal::Numeric(n))
        },
        Token::String(s) => {
            let tag = if let Some(token) = peek(lexer)? {
                let (token, mut tag_span) = token.into_raw_parts();
                match token {
                    Token::Keyword(Keyword::LangTag(lang)) => {
                        expect(lexer, &mut span)?;
                        Some(Loc::new(Tag::Lang(lang), tag_span))
                    },
                    Token::Keyword(Keyword::IriTag) => {
                        expect(lexer, &mut span)?;
                        let (token, token_span) = expect(lexer, &mut span)?.into_raw_parts();
                        let iri = match token {
                            Token::Ident(id) => {
                                IriRef::curie_from_str(id.as_str())
                            },
                            Token::Iri(iri) => {
                                IriRef::Iri(iri)
                            },
                            unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
                        };
                        tag_span.append(token_span);
                        Some(Loc::new(Tag::Iri(iri), tag_span))
                    },
                    _ => None
                }
            } else {
                None
            };
            Object::Literal(Literal::String(s, tag))
        },
        Token::Keyword(Keyword::True) => {
            Object::Literal(Literal::Boolean(true))
        },
        Token::Keyword(Keyword::False) => {
            Object::Literal(Literal::Boolean(false))
        },
        Token::Ident(id) => {
            Object::from(id.as_str())
        },
        Token::Iri(iri) => {
            Object::Iri(IriRef::Iri(iri))
        },
        Token::Group(Delimiter::Bracket, group) => {
            let mut lexer = group.into_iter().map(safe_token).peekable();
            let (inner_list, object_span) = parse_verb_objects_list(&mut lexer, span.end())?.into_raw_parts();
            span.append(object_span);
            Object::Anonymous(inner_list)
        },
        Token::Group(Delimiter::Parenthesis, group) => {
            let mut objects = Vec::new();
            let mut lexer = group.into_iter().map(safe_token).peekable();
            while let Some(_) = lexer.peek() {
                let object = parse_object(&mut lexer, span.end())?;
                span.append(object.span());
                objects.push(object);
            }
            Object::Collection(objects)
        },
        unexpected => return Err(Loc::new(Error::UnexpectedToken(unexpected), token_span))
    };

    Ok(Loc::new(object, span))
}

fn parse_objects<L: Iterator<Item = lexer::Result<Loc<Token>>>>(lexer: &mut Peekable<L>, pos: Position) -> Result<Loc<Vec<Loc<Object>>>> {
    let mut span: Span = pos.into();
    let mut objects = Vec::new();
    loop {
        if let Some(token) = peek(lexer)? {
            let (token, _) = token.into_raw_parts();
            match token {
                Token::Punct('.') | Token::Punct(';') => break,
                Token::Punct(',') => {
                    consume(lexer, &mut span)?;
                },
                _ => {
                    let object = parse_object(lexer, span.end())?;
                    span.append(object.span());
                    objects.push(object);
                }
            }
        } else {
            if objects.is_empty() {
                return Err(Loc::new(Error::UnexpectedEos, span))
            } else {
                break
            }
        }
    }

    Ok(Loc::new(objects, span))
}
