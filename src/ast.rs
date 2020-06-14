//! Syntax elements of Turtle.

use source_span::Loc;
use iref::{
	Iri,
	IriBuf
};

/// A blank node name.
pub type BlankNode = String;

/// An IRI or compact IRI.
#[derive(Clone, Debug)]
pub enum IriRef {
	Iri(IriBuf),
	Curie(Option<String>, String)
}

impl IriRef {
	pub fn curie_from_str(s: &str) -> IriRef {
		if let Some(i) = s.find(':') {
			let (prefix, id) = s.split_at(i);
			let (_, id) = id.split_at(1);
			IriRef::Curie(Some(prefix.to_string()), id.to_string())
		} else {
			IriRef::Curie(None, s.to_string())
		}
	}
}

/// A Turtle document.
pub struct Document {
	pub statements: Vec<Loc<Statement>>
}

impl Document {
	pub fn base_prefix(&self) -> Option<Iri> {
		for stm in &self.statements {
			match stm.as_ref() {
				Statement::Directive(Directive::Base(iri)) => return Some(iri.as_iri()),
				_ => ()
			}
		}

		None
	}
}

/// A statement (directive of triples declaration).
pub enum Statement {
	/// Directive.
	Directive(Directive),

	/// Triples declaration.
	Triples(Loc<Subject>, Loc<Vec<Loc<VerbObjects>>>),
}

/// A directive.
pub enum Directive {
	/// `@prefix` directive.
	Prefix(Loc<String>, Loc<IriBuf>),

	/// `@base` directive.
	Base(Loc<IriBuf>)
}

/// verb-objects part of a triples declaration.
pub struct VerbObjects {
	pub verb: Loc<Verb>,
	pub objects: Loc<Vec<Loc<Object>>>
}

/// Verb (either `a` or a predicate).
#[derive(Debug)]
pub enum Verb {
	/// `a` keyword.
	A,

	/// Predicate.
	Predicate(IriRef)
}

impl<'a> From<&'a str> for Verb {
	fn from(s: &'a str) -> Verb {
		if s == "a" {
			Verb::A
		} else {
			if let Some(i) = s.find(':') {
				let (prefix, id) = s.split_at(i);
				let (_, id) = id.split_at(1);
				Verb::Predicate(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
			} else {
				Verb::Predicate(IriRef::Curie(None, s.to_string()))
			}
		}
	}
}

/// Subject of a triples declaration.
pub enum Subject {
	/// IRI or compact IRI.
	Iri(IriRef),

	/// Blank node.
	BlankNode(BlankNode),

	/// Collection of subjects.
	Collection(Vec<Loc<Subject>>),

	/// No subject.
	Blank(Vec<Loc<VerbObjects>>)
}

impl<'a> From<&'a str> for Subject {
	fn from(s: &'a str) -> Subject {
		if let Some(i) = s.find(':') {
			let (prefix, id) = s.split_at(i);
			if prefix == "_" {
				// println!("bl: {}", s);
				Subject::BlankNode(id.to_string())
			} else {
				let (_, id) = id.split_at(1);
				Subject::Iri(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
			}
		} else {
			Subject::Iri(IriRef::Curie(None, s.to_string()))
		}
	}
}

/// Object of a triples declaration.
pub enum Object {
	/// IRI or compact IRI.
	Iri(IriRef),

	/// Blank node.
	BlankNode(BlankNode),

	/// Collection of objects.
	Collection(Vec<Loc<Object>>),

	/// No object.
	Blank(Vec<Loc<VerbObjects>>),

	/// Literal value.
	Literal(Literal)
}

impl<'a> From<&'a str> for Object {
	fn from(s: &'a str) -> Object {
		if let Some(i) = s.find(':') {
			let (prefix, id) = s.split_at(i);
			if prefix == "_" {
				Object::BlankNode(id.to_string())
			} else {
				let (_, id) = id.split_at(1);
				Object::Iri(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
			}
		} else {
			Object::Iri(IriRef::Curie(None, s.to_string()))
		}
	}
}

/// Literal value.
pub enum Literal {
	/// Maybe tagged string.
	String(String, Option<Loc<Tag>>),

	/// NUmerical value.
	Numeric(Numeric),

	/// Boolean value.
	Boolean(bool)
}

/// String tag.
pub enum Tag {
	/// Lang tag for lang strings.
	Lang(String),

	/// IRI tag.
	Iri(IriRef)
}

/// Numerical value.
#[derive(Clone, Copy, Debug)]
pub enum Numeric {
	/// Integer.
	Int(i64),

	/// Decimal value.
	Decimal(bool, u32, u32), // sign, integer part, decimal part

	/// Double precision decimal value.
	Double(bool, u32, u32, i32) // sign, integer part, decimal part, exponent
}
