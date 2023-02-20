use iref::{Iri, IriBuf, IriRef, IriRefBuf};
use locspan::Meta;
use rdf_types::{
	BlankIdVocabulary, Generator, IriVocabulary, IriVocabularyMut, Triple, Vocabulary,
	VocabularyMut,
};
use static_iref::iri;
use std::collections::HashMap;

const RDF_TYPE: Iri<'static> = iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
const RDF_NIL: Iri<'static> = iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
const RDF_FIRST: Iri<'static> = iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#first");
const RDF_REST: Iri<'static> = iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest");
const XSD_BOOLEAN: Iri<'static> = iri!("http://www.w3.org/2001/XMLSchema#boolean");
const XSD_INTEGER: Iri<'static> = iri!("http://www.w3.org/2001/XMLSchema#integer");
const XSD_DECIMAL: Iri<'static> = iri!("http://www.w3.org/2001/XMLSchema#decimal");
const XSD_DOUBLE: Iri<'static> = iri!("http://www.w3.org/2001/XMLSchema#double");

/// Triple with metadata.
pub type MetaTriple<M, V = ()> = Meta<
	Triple<
		Meta<rdf_types::Subject<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>, M>,
		Meta<<V as IriVocabulary>::Iri, M>,
		Meta<
			rdf_types::meta::Object<
				M,
				<V as IriVocabulary>::Iri,
				<V as BlankIdVocabulary>::BlankId,
			>,
			M,
		>,
	>,
	M,
>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("cannot resolve relative IRI <{0}>: no base IRI")]
	NoBaseIri(IriRefBuf),

	#[error("unknown IRI prefix `{0}`")]
	UnknownPrefix(String),

	#[error("invalid compact IRI suffix in `{prefix}:{invalid_suffix}`")]
	InvalidCompactIriSuffix {
		prefix: String,
		iri: IriBuf,
		invalid_suffix: String,
	},
}

pub type MetaError<M> = Meta<Box<Error>, M>;

impl<M: Clone> crate::Document<M> {
	pub fn build_triples(
		&self,
		base_iri: Option<IriBuf>,
		mut generator: impl Generator<()>,
	) -> Result<Vec<MetaTriple<M, ()>>, MetaError<M>> {
		let mut triples = Vec::new();
		let mut context = Context::new(
			base_iri,
			rdf_types::vocabulary::no_vocabulary_mut(),
			&mut generator,
		);
		self.build(&mut context, &mut triples)?;
		Ok(triples)
	}

	pub fn build_triples_with<V: VocabularyMut>(
		&self,
		base_iri: Option<V::Iri>,
		vocabulary: &mut V,
		mut generator: impl Generator<V>,
	) -> Result<Vec<MetaTriple<M, V>>, MetaError<M>>
	where
		V::Iri: Clone,
		V::BlankId: Clone,
	{
		let mut triples = Vec::new();
		let mut context = Context::new(base_iri, vocabulary, &mut generator);
		self.build(&mut context, &mut triples)?;
		Ok(triples)
	}
}

pub struct Context<'v, 'g, M, V: IriVocabulary, G> {
	vocabulary: &'v mut V,
	generator: &'g mut G,
	base_iri: Option<V::Iri>,
	prefixes: HashMap<String, Meta<V::Iri, M>>,
}

impl<'v, 'g, M, V: IriVocabulary, G> Context<'v, 'g, M, V, G> {
	pub fn new(base_iri: Option<V::Iri>, vocabulary: &'v mut V, generator: &'g mut G) -> Self {
		Self {
			vocabulary,
			generator,
			base_iri,
			prefixes: HashMap::new(),
		}
	}

	pub fn resolve_iri_ref(
		&mut self,
		Meta(iri_ref, meta): Meta<IriRef, &M>,
	) -> Result<V::Iri, MetaError<M>>
	where
		M: Clone,
		V: IriVocabularyMut,
	{
		match &self.base_iri {
			Some(current) => {
				let iri = iri_ref.resolved(self.vocabulary.iri(current).unwrap());
				Ok(self.vocabulary.insert(iri.as_iri()))
			}
			None => match iri_ref.into_iri() {
				Ok(iri) => Ok(self.vocabulary.insert(iri)),
				Err(_) => Err(Meta(
					Box::new(Error::NoBaseIri(iri_ref.to_owned())),
					meta.clone(),
				)),
			},
		}
	}

	pub fn resolve_compact_iri(
		&mut self,
		prefix: Meta<&str, &M>,
		suffix: Meta<&str, &M>,
		meta: &M,
	) -> Result<V::Iri, MetaError<M>>
	where
		M: Clone,
		V: IriVocabularyMut,
	{
		match self.prefixes.get(prefix.0) {
			Some(iri) => {
				let iri = self.vocabulary.iri(iri).unwrap();
				let mut buffer = iri.to_string();
				buffer.push_str(suffix.0);
				match Iri::new(&buffer) {
					Ok(result) => Ok(self.vocabulary.insert(result)),
					Err(_) => Err(Meta(
						Box::new(Error::InvalidCompactIriSuffix {
							prefix: prefix.0.to_owned(),
							iri: iri.to_owned(),
							invalid_suffix: suffix.0.to_owned(),
						}),
						meta.clone(),
					)),
				}
			}
			None => Err(Meta(
				Box::new(Error::UnknownPrefix(prefix.0.to_owned())),
				prefix.1.clone(),
			)),
		}
	}

	pub fn insert_prefix(&mut self, prefix: String, iri: V::Iri, meta: M) {
		self.prefixes.insert(prefix, Meta(iri, meta));
	}
}

pub trait Build<M, V: VocabularyMut, G> {
	fn build(
		&self,
		context: &mut Context<M, V, G>,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<(), MetaError<M>>;
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> Build<M, V, G> for crate::Document<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	fn build(
		&self,
		context: &mut Context<M, V, G>,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<(), MetaError<M>> {
		for statement in &self.statements {
			match statement {
				Meta(crate::Statement::Directive(directive), meta) => match directive {
					crate::Directive::Base(iri) | crate::Directive::SparqlBase(iri) => {
						let iri_ref = iri.borrow().map(IriRefBuf::as_iri_ref);
						context.base_iri = Some(context.resolve_iri_ref(iri_ref)?);
					}
					crate::Directive::Prefix(prefix, iri)
					| crate::Directive::SparqlPrefix(prefix, iri) => {
						let iri_ref = iri.borrow().map(IriRefBuf::as_iri_ref);
						let iri = context.resolve_iri_ref(iri_ref)?;
						context.insert_prefix(prefix.value().clone(), iri, meta.clone());
					}
				},
				Meta(crate::Statement::Triples(t), meta) => {
					t.build(context, meta, triples)?;
				}
			}
		}

		Ok(())
	}
}

impl<M: Clone> crate::Triples<M> {
	fn build<V: VocabularyMut, G: Generator<V>>(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<(), MetaError<M>>
	where
		V::Iri: Clone,
		V::BlankId: Clone,
	{
		let subject = self.subject.build(context, triples)?;

		for Meta(po_list, _) in self.predicate_objects_list.iter() {
			po_list.build(context, meta, &subject, triples)?;
		}

		Ok(())
	}
}

impl<M: Clone> crate::PredicateObjects<M> {
	fn build<V: VocabularyMut, G: Generator<V>>(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		subject: &Meta<rdf_types::Subject<V::Iri, V::BlankId>, M>,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<(), MetaError<M>>
	where
		V::Iri: Clone,
		V::BlankId: Clone,
	{
		let predicate = self.verb.build(context, triples)?;

		for o in &self.objects.value().0 {
			let object = o.build(context, triples)?;
			triples.push(Meta(
				rdf_types::Triple(subject.clone(), predicate.clone(), object),
				meta.clone(),
			))
		}

		Ok(())
	}
}

trait BuildFragment<M, V: Vocabulary, G> {
	type Target;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>>;
}

impl<T: BuildMetaFragment<M, V, G>, M: Clone, V: VocabularyMut, G> BuildFragment<M, V, G>
	for Meta<T, M>
{
	type Target = Meta<T::Target, M>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		Ok(Meta(
			self.value().build(context, self.metadata(), triples)?,
			self.metadata().clone(),
		))
	}
}

trait BuildMetaFragment<M, V: Vocabulary, G> {
	type Target;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>>;
}

impl<M: Clone, V: VocabularyMut, G> BuildMetaFragment<M, V, G> for crate::Iri<M> {
	type Target = V::Iri;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		_triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::IriRef(iri_ref) => context.resolve_iri_ref(Meta(iri_ref.as_iri_ref(), meta)),
			Self::Compact(prefix, suffix) => context.resolve_compact_iri(
				prefix.borrow().map(String::as_str),
				suffix.borrow().map(String::as_str),
				meta,
			),
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G> for crate::BlankNode<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::Subject<V::Iri, V::BlankId>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::Label(b) => Ok(rdf_types::Subject::Blank(
				context.vocabulary.insert_blank_id(b),
			)),
			Self::Anonymous(b_property_list) => {
				let b = Meta(context.generator.next(context.vocabulary), meta.clone());

				for predicate_objects in b_property_list.iter() {
					predicate_objects.build(context, meta, &b, triples)?;
				}

				Ok(b.0)
			}
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G> for crate::Subject<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::Subject<V::Iri, V::BlankId>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::Iri(iri) => Ok(rdf_types::Subject::Iri(iri.build(context, meta, triples)?)),
			Self::BlankNode(b) => Ok(b.build(context, meta, triples)?),
			Self::Collection(collection) => collection.build(context, meta, triples),
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G>
	for crate::Collection<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::Subject<V::Iri, V::BlankId>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		let mut head = rdf_types::Subject::Iri(context.vocabulary.insert(RDF_NIL));

		for o in self.0.iter().rev() {
			let item = o.build(context, triples)?;
			let node = context.generator.next(context.vocabulary);

			triples.push(Meta(
				rdf_types::Triple(
					Meta(node.clone(), item.metadata().clone()),
					Meta(context.vocabulary.insert(RDF_REST), item.metadata().clone()),
					Meta(head.into_term(), item.metadata().clone()),
				),
				meta.clone(),
			));

			triples.push(Meta(
				rdf_types::Triple(
					Meta(node.clone(), item.metadata().clone()),
					Meta(
						context.vocabulary.insert(RDF_FIRST),
						item.metadata().clone(),
					),
					item,
				),
				meta.clone(),
			));

			head = node;
		}

		Ok(head)
	}
}

impl<M: Clone, V: VocabularyMut, G> BuildMetaFragment<M, V, G> for crate::Verb<M> {
	type Target = V::Iri;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::A => Ok(context.vocabulary.insert(RDF_TYPE)),
			Self::Predicate(i) => i.build(context, meta, triples),
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G> for crate::Object<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::meta::Object<M, V::Iri, V::BlankId>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::Iri(iri) => Ok(rdf_types::Object::Iri(iri.build(context, meta, triples)?)),
			Self::BlankNode(b) => Ok(b.build(context, meta, triples)?.into_term()),
			Self::Collection(collection) => {
				Ok(collection.build(context, meta, triples)?.into_term())
			}
			Self::Literal(literal) => Ok(rdf_types::Object::Literal(
				literal.build(context, meta, triples)?,
			)),
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G> for crate::Literal<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::meta::Literal<M, rdf_types::StringLiteral, V::Iri>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::Boolean(b) => b.build(context, meta, triples),
			Self::Numeric(n) => n.build(context, meta, triples),
			Self::Rdf(literal) => literal.build(context, meta, triples),
		}
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G> for bool
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::meta::Literal<M, rdf_types::StringLiteral, V::Iri>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		_triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		let s = if *self { "true" } else { "false" };

		Ok(rdf_types::meta::Literal::TypedString(
			Meta(s.to_owned().into(), meta.clone()),
			Meta(context.vocabulary.insert(XSD_BOOLEAN), meta.clone()),
		))
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G>
	for crate::NumericLiteral
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::meta::Literal<M, rdf_types::StringLiteral, V::Iri>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		meta: &M,
		_triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		let (s, ty) = match self {
			Self::Integer(i) => (i.as_str(), XSD_INTEGER),
			Self::Decimal(d) => (d.as_str(), XSD_DECIMAL),
			Self::Double(d) => (d.as_str(), XSD_DOUBLE),
		};

		Ok(rdf_types::meta::Literal::TypedString(
			Meta(s.to_owned().into(), meta.clone()),
			Meta(context.vocabulary.insert(ty), meta.clone()),
		))
	}
}

impl<M: Clone, V: VocabularyMut, G: Generator<V>> BuildMetaFragment<M, V, G>
	for crate::RdfLiteral<M>
where
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Target = rdf_types::meta::Literal<M, rdf_types::StringLiteral, V::Iri>;

	fn build(
		&self,
		context: &mut Context<M, V, G>,
		_meta: &M,
		triples: &mut Vec<MetaTriple<M, V>>,
	) -> Result<Self::Target, MetaError<M>> {
		match self {
			Self::String(s) => Ok(rdf_types::meta::Literal::String(s.clone())),
			Self::LangString(s, t) => {
				Ok(rdf_types::meta::Literal::LangString(s.clone(), t.clone()))
			}
			Self::TypedString(s, t) => Ok(rdf_types::meta::Literal::TypedString(
				s.clone(),
				t.build(context, triples)?,
			)),
		}
	}
}
