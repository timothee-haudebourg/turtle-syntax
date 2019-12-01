use crate::Loc;

pub type Iri = String;
pub type BlankNode = String;

#[derive(Clone)]
pub enum IriRef {
    Iri(Iri),
    Curie(Option<String>, String)
}

impl IriRef {
    pub fn curie_from_str(s: &str) -> IriRef {
        if let Some(i) = s.find(':') {
            let (prefix, id) = s.split_at(i);
            IriRef::Curie(Some(prefix.to_string()), id.to_string())
        } else {
            IriRef::Curie(None, s.to_string())
        }
    }
}

pub struct Document {
    pub statements: Vec<Loc<Statement>>
}

pub enum Statement {
    Directive(Directive),
    Triples(Loc<Subject>, Loc<Vec<Loc<VerbObjects>>>),
}

pub enum Directive {
    Prefix(Loc<String>, Loc<Iri>),
    Base(Loc<Iri>)
}

pub struct VerbObjects {
    pub verb: Loc<Verb>,
    pub objects: Loc<Vec<Loc<Object>>>
}

pub enum Verb {
    A,
    Predicate(IriRef)
}

impl<'a> From<&'a str> for Verb {
    fn from(s: &'a str) -> Verb {
        if s == "a" {
            Verb::A
        } else {
            if let Some(i) = s.find(':') {
                let (prefix, id) = s.split_at(i);
                Verb::Predicate(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
            } else {
                Verb::Predicate(IriRef::Curie(None, s.to_string()))
            }
        }
    }
}

pub enum Subject {
    Iri(IriRef),
    BlankNode(BlankNode),
    Collection(Vec<Loc<Subject>>),
    Anonymous(Vec<Loc<VerbObjects>>)
}

impl<'a> From<&'a str> for Subject {
    fn from(s: &'a str) -> Subject {
        if let Some(i) = s.find(':') {
            let (prefix, id) = s.split_at(i);
            if prefix == "_" {
                Subject::BlankNode(id.to_string())
            } else {
                Subject::Iri(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
            }
        } else {
            Subject::Iri(IriRef::Curie(None, s.to_string()))
        }
    }
}

pub enum Object {
    Iri(IriRef),
    BlankNode(BlankNode),
    Collection(Vec<Loc<Object>>),
    Anonymous(Vec<Loc<VerbObjects>>),
    Literal(Literal)
}

impl<'a> From<&'a str> for Object {
    fn from(s: &'a str) -> Object {
        if let Some(i) = s.find(':') {
            let (prefix, id) = s.split_at(i);
            if prefix == "_" {
                Object::BlankNode(id.to_string())
            } else {
                Object::Iri(IriRef::Curie(Some(prefix.to_string()), id.to_string()))
            }
        } else {
            Object::Iri(IriRef::Curie(None, s.to_string()))
        }
    }
}

pub enum Literal {
    String(String, Option<Loc<Tag>>),
    Numeric(Numeric),
    Boolean(bool)
}

#[derive(Clone)]
pub enum Tag {
    Lang(String),
    Iri(IriRef)
}

#[derive(Clone, Copy)]
pub enum Numeric {
    Int(i64),
    Decimal(bool, u32, u32), // sign, integer part, decimal part
    Double(bool, u32, u32, i32) // sign, integer part, decimal part, exponent
}
