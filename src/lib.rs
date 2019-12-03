extern crate curie;
extern crate ordered_float;

mod location;
mod syntax;

use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use curie::{Curie, PrefixMapping};
use source_span::Span;
pub use location::*;
pub use syntax::*;

pub enum Error {
    InvalidPrefix(String),
    UnknownPrefix(String),
    NoDefaultPrefix,
    InvalidLiteral
}

struct Context<'a> {
    ds: &'a mut grdf::Dataset,
    prefixes: PrefixMapping,
    nammed_blank_nodes: HashMap<BlankNode, usize>,
    blank_node_count: usize
}

impl<'a> TryFrom<&'a Loc<Document>> for grdf::Dataset {
    type Error = Loc<Error>;

    fn try_from(doc: &'a Loc<Document>) -> Result<grdf::Dataset, Loc<Error>> {
        let mut ds = grdf::Dataset::default();
        let mut ctx = Context {
            ds: &mut ds,
            prefixes: PrefixMapping::default(),
            nammed_blank_nodes: HashMap::new(),
            blank_node_count: 0
        };

        for stm in &doc.statements {
            match stm.as_ref() {
                Statement::Directive(Directive::Prefix(name, iri)) => {
                    if let Err(_) = ctx.prefixes.add_prefix(name.as_str(), iri.as_str()) {
                        return Err(Loc::new(Error::InvalidPrefix(name.as_ref().clone()), name.span()))
                    }
                },
                Statement::Directive(Directive::Base(iri)) => {
                    ctx.prefixes.set_default(iri.as_str())
                },
                _ => ()
            }
        }

        for stm in &doc.statements {
            match stm.as_ref() {
                Statement::Triples(subject, verb_objects_list) => {
                    ctx.add_triples(subject, verb_objects_list.as_ref())?
                },
                _ => ()
            }
        }

        Ok(ds)
    }
}

impl<'a> Context<'a> {
    pub fn next_id(&mut self) -> usize {
        let id = self.blank_node_count;
        self.blank_node_count += 1;
        id
    }

    pub fn alloc_blank_node(&mut self) -> grdf::Node {
        grdf::Node::Anonymous(self.next_id())
    }

    pub fn nammed_blank_node(&mut self, id: &str) -> grdf::Node {
        if let Some(id) = self.nammed_blank_nodes.get(id) {
            grdf::Node::Anonymous(*id)
        } else {
            let id = self.next_id();
            self.nammed_blank_nodes.insert(id.to_string(), id);
            grdf::Node::Anonymous(id)
        }
    }

    pub fn expand_iri(&self, r: &IriRef, span: Span) -> Result<String, Loc<Error>> {
        match r {
            IriRef::Iri(iri) => {
                Ok(iri.clone())
            },
            IriRef::Curie(prefix, id) => {
                let curie = Curie::new(match prefix {
                    Some(prefix) => Some(prefix.as_str()),
                    None => None
                }, id.as_str());
                match self.prefixes.expand_curie(&curie) {
                    Ok(iri) => Ok(iri),
                    Err(curie::ExpansionError::Invalid) => Err(Loc::new(Error::UnknownPrefix(prefix.clone().unwrap()), span)),
                    Err(curie::ExpansionError::MissingDefault) => Err(Loc::new(Error::NoDefaultPrefix, span))
                }
            }
        }
    }

    pub fn iri_to_node(&self, r: &IriRef, span: Span) -> Result<grdf::Node, Loc<Error>> {
        Ok(grdf::Node::Nammed(self.expand_iri(r, span)?))
    }

    pub fn add_triples(&mut self, subject: &Loc<Subject>, verb_objects_list: &Vec<Loc<VerbObjects>>) -> Result<(), Loc<Error>> {
        match subject.as_ref() {
            Subject::Iri(iri) => {
                let subject_node = self.iri_to_node(iri, subject.span())?;
                self.assign_verb_objects_list(&subject_node, verb_objects_list)?;
            },
            Subject::BlankNode(id) => {
                let subject_node = self.nammed_blank_node(id.as_str());
                self.assign_verb_objects_list(&subject_node, verb_objects_list)?;
            },
            Subject::Collection(subjects) => {
                for subject in subjects {
                    self.add_triples(subject, verb_objects_list)?;
                }
            },
            Subject::Anonymous(inner_verb_objects_list) => {
                let subject_node = self.alloc_blank_node();
                self.assign_verb_objects_list(&subject_node, inner_verb_objects_list)?;
                self.assign_verb_objects_list(&subject_node, verb_objects_list)?;
            }
        }

        Ok(())
    }

    pub fn assign_verb_objects_list(&mut self, subject: &grdf::Node, verb_objects_list: &Vec<Loc<VerbObjects>>) -> Result<(), Loc<Error>> {
        for verb_objects in verb_objects_list {
            let predicate = match verb_objects.verb.as_ref() {
                Verb::A => grdf::Node::Nammed("http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string()),
                Verb::Predicate(iri) => {
                    self.iri_to_node(iri, verb_objects.verb.span())?
                }
            };

            for object in verb_objects.objects.as_ref() {
                self.assign_predicate_object(subject, &predicate, object)?;
            }
        }

        Ok(())
    }

    pub fn assign_predicate_object(&mut self, subject: &grdf::Node, predicate: &grdf::Node, object: &Loc<Object>) -> Result<(), Loc<Error>> {
        match object.as_ref() {
            Object::Iri(iri) => {
                let object_node = self.iri_to_node(iri, object.span())?;
                self.add_triple(subject, predicate, grdf::Object::Entity(object_node))?;
            },
            Object::BlankNode(id) => {
                let object_node = self.nammed_blank_node(id.as_str());
                self.add_triple(subject, predicate, grdf::Object::Entity(object_node))?;
            },
            Object::Collection(objects) => {
                for object in objects {
                    self.assign_predicate_object(subject, predicate, object)?;
                }
            },
            Object::Anonymous(inner_verb_objects_list) => {
                let object_node = self.alloc_blank_node();
                self.assign_verb_objects_list(&object_node, inner_verb_objects_list)?;
                self.add_triple(subject, predicate, grdf::Object::Entity(object_node))?;
            },
            Object::Literal(l) => {
                let object_node = match l {
                    Literal::String(s, Some(tag)) => {
                        let tag = match tag.as_ref() {
                            Tag::Lang(lang) => grdf::Tag::Lang(lang.clone()),
                            Tag::Iri(iri) => {
                                let iri = self.expand_iri(iri, tag.span())?;
                                grdf::Tag::Iri(iri)
                            }
                        };
                        grdf::Literal::String(s.clone(), Some(tag))
                    },
                    Literal::String(s, None) => {
                        grdf::Literal::String(s.clone(), None)
                    }
                    Literal::Numeric(n) => {
                        match n.try_into() {
                            Ok(l) => l,
                            Err(_) => return Err(Loc::new(Error::InvalidLiteral, object.span()))
                        }
                    },
                    Literal::Boolean(b) => grdf::Literal::Bool(*b)
                };

                self.add_triple(subject, predicate, grdf::Object::Value(object_node))?;
            }
        }

        Ok(())
    }

    pub fn add_triple(&mut self, subject: &grdf::Node, predicate: &grdf::Node, object: grdf::Object) -> Result<(), Loc<Error>> {
        self.ds.add(None, subject.clone(), predicate.clone(), object);
        Ok(())
    }
}
