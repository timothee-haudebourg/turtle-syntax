//! Syntax elements of Turtle.
use std::fmt;

use iref::IriRefBuf;
use locspan::Meta;
pub use rdf_types::{BlankId, BlankIdBuf};
pub use xsd_types::lexical::{DecimalBuf, DoubleBuf, IntegerBuf};

pub type RdfLiteral<M> = rdf_types::meta::Literal<M, String, Iri<M>>;

/// An IRI or compact IRI.
#[derive(Clone, Debug)]
pub enum Iri<M> {
	IriRef(IriRefBuf),
	Compact(Meta<String, M>, Meta<String, M>),
}

/// A Turtle document.
#[derive(Clone, Debug)]
pub struct Document<M> {
	pub statements: Vec<Meta<Statement<M>, M>>,
}

impl<M> Default for Document<M> {
	fn default() -> Self {
		Self {
			statements: Vec::new(),
		}
	}
}

impl<M> Document<M> {
	pub fn new() -> Document<M> {
		Self::default()
	}

	pub fn insert(&mut self, statement: Meta<Statement<M>, M>) {
		self.statements.push(statement)
	}
}

/// A statement (directive of triples declaration).
#[derive(Clone, Debug)]
pub enum Statement<M> {
	/// Directive.
	Directive(Directive<M>),

	/// Triples declaration.
	Triples(Triples<M>),
}

#[derive(Clone, Debug)]
pub struct Triples<M> {
	pub subject: Meta<Subject<M>, M>,
	pub predicate_objects_list: Meta<PredicateObjectsList<M>, M>,
}

pub type PredicateObjectsList<M> = Vec<Meta<PredicateObjects<M>, M>>;

/// A directive.
#[derive(Clone, Debug)]
pub enum Directive<M> {
	/// `@prefix` directive.
	Prefix(Meta<String, M>, Meta<IriRefBuf, M>),

	/// `@base` directive.
	Base(Meta<IriRefBuf, M>),

	/// SPARQL `PREFIX` directive.
	SparqlPrefix(Meta<String, M>, Meta<IriRefBuf, M>),

	/// SPARQL `BASE` directive.
	SparqlBase(Meta<IriRefBuf, M>),
}

/// Verb (either `a` or a predicate).
#[derive(Clone, Debug)]
pub enum Verb<M> {
	/// `a` keyword.
	A,

	/// Predicate.
	Predicate(Iri<M>),
}

/// Subject of a triples declaration.
#[derive(Clone, Debug)]
pub enum Subject<M> {
	/// IRI or compact IRI.
	Iri(Iri<M>),

	/// Blank node.
	BlankNode(BlankNode<M>),

	/// Collection of subjects.
	Collection(Collection<M>),
}

/// Collection of objects.
#[derive(Clone, Debug)]
pub struct Collection<M>(pub Vec<Meta<Object<M>, M>>);

#[derive(Clone, Debug)]
pub enum BlankNode<M> {
	Label(BlankIdBuf),
	Anonymous(Meta<BlankNodePropertyList<M>, M>),
}

pub type BlankNodePropertyList<M> = PredicateObjectsList<M>;

/// Object of a triples declaration.
#[derive(Clone, Debug)]
pub enum Object<M> {
	/// IRI or compact IRI.
	Iri(Iri<M>),

	/// Blank node.
	BlankNode(BlankNode<M>),

	/// Collection of objects.
	Collection(Collection<M>),

	/// Literal value.
	Literal(Literal<M>),
}

#[derive(Clone, Debug)]
pub struct PredicateObjects<M> {
	pub verb: Meta<Verb<M>, M>,
	pub objects: Meta<Objects<M>, M>,
}

/// Non empty list of objects.
#[derive(Clone, Debug)]
pub struct Objects<M>(pub Vec<Meta<Object<M>, M>>);

/// Literal value.
#[derive(Clone, Debug)]
pub enum Literal<M> {
	/// RDF literal.
	Rdf(RdfLiteral<M>),

	/// Numeric literal.
	Numeric(NumericLiteral),

	/// Boolean literal.
	Boolean(bool),
}

/// Numeric literal value.
#[derive(Clone, Debug)]
pub enum NumericLiteral {
	Integer(IntegerBuf),
	Decimal(DecimalBuf),
	Double(DoubleBuf),
}

impl fmt::Display for NumericLiteral {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Integer(i) => i.fmt(f),
			Self::Decimal(d) => d.fmt(f),
			Self::Double(d) => d.fmt(f),
		}
	}
}
