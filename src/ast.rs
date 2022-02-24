//! Syntax elements of Turtle.
use iref::IriRefBuf;
use locspan::Loc;
use langtag::LanguageTagBuf;
pub use rdf_types::{BlankId, BlankIdBuf, StringLiteral};
pub use xsd_types::{Double, DoubleBuf};

/// An IRI or compact IRI.
#[derive(Clone, Debug)]
pub enum Iri<F> {
	IriRef(IriRefBuf),
	Compact(Option<Loc<String, F>>, Loc<String, F>),
}

/// A Turtle document.
#[derive(Clone, Debug)]
pub struct Document<F> {
	pub statements: Vec<Loc<Statement<F>, F>>,
}

impl<F> Document<F> {
	pub fn base_prefix(&self) -> Option<iref::IriRef> {
		for stm in &self.statements {
			if let Statement::Directive(Directive::Base(iri)) = stm.as_ref() {
				return Some(iri.as_iri_ref());
			}
		}

		None
	}
}

/// A statement (directive of triples declaration).
#[derive(Clone, Debug)]
pub enum Statement<F> {
	/// Directive.
	Directive(Directive<F>),

	/// Triples declaration.
	Triples(Loc<Subject<F>, F>, Vec<Loc<PredicateObjects<F>, F>>),
}

/// A directive.
#[derive(Clone, Debug)]
pub enum Directive<F> {
	/// `@prefix` directive.
	Prefix(Loc<String, F>, Loc<IriRefBuf, F>),

	/// `@base` directive.
	Base(Loc<IriRefBuf, F>),

	/// SPARQL `PREFIX` directive.
	SparqlPrefix(Loc<String, F>, Loc<IriRefBuf, F>),

	/// SPARQL `BASE` directive.
	SparqlBase(Loc<IriRefBuf, F>),
}

/// Verb (either `a` or a predicate).
#[derive(Clone, Debug)]
pub enum Verb<F> {
	/// `a` keyword.
	A,

	/// Predicate.
	Predicate(Iri<F>),
}

#[derive(Clone, Debug)]
pub enum BlankNode<F> {
	Label(BlankIdBuf),
	Anonymous(Vec<Loc<PredicateObjects<F>, F>>),
}

/// Subject of a triples declaration.
#[derive(Clone, Debug)]
pub enum Subject<F> {
	/// IRI or compact IRI.
	Iri(Iri<F>),

	/// Blank node.
	BlankNode(BlankNode<F>),

	/// Collection of subjects.
	Collection(Vec<Loc<Object<F>, F>>),
}

/// Object of a triples declaration.
#[derive(Clone, Debug)]
pub enum Object<F> {
	/// IRI or compact IRI.
	Iri(Iri<F>),

	/// Blank node.
	BlankNode(BlankNode<F>),

	/// Collection of objects.
	Collection(Vec<Loc<Self, F>>),

	/// Literal value.
	Literal(Literal<F>),
}

#[derive(Clone, Debug)]
pub struct PredicateObjects<F> {
	pub verb: Loc<Verb<F>, F>,
	pub objects: Loc<Vec<Loc<Object<F>, F>>, F>,
}

/// Literal value.
#[derive(Clone, Debug)]
pub enum Literal<F> {
	Rdf(RdfLiteral<F>),

	/// Numerical value.
	Number(DoubleBuf),

	/// Boolean value.
	Boolean(bool),
}

/// RDF Literal.
#[derive(Clone, Debug)]
pub enum RdfLiteral<F> {
	/// Untyped string literal.
	String(Loc<StringLiteral, F>),

	/// Typed string literal.
	TypedString(Loc<StringLiteral, F>, Loc<Iri<F>, F>),

	/// Language string.
	LangString(Loc<StringLiteral, F>, Loc<LanguageTagBuf, F>),
}