pub type Iri = String;
pub type BlankNode = String;

pub enum IriRef {
    Iri(Iri),
    Curie(Option<String>, String)
}

pub struct Document {
    statements: Vec<Loc<Statement>>
}

pub enum Statement {
    Directive(Directive),
    Triples(Loc<Subject>, Vec<Loc<VerbObjects>>),
}

pub enum Directive {
    Prefix(Loc<String>, Loc<Iri>),
    Base(Loc<Iri>)
}

pub struct VerbObjects {
    verb: Loc<Verb>,
    objects: Vec<Loc<Object>>
}

pub enum Verb {
    A,
    Predicate(Predicate)
}

pub enum Subject {
    Iri(IriRef),
    BlankNode(BlankNode),
    Collection(Vec<Loc<Subject>>),
    Anonymous(Vec<Loc<VerbObjects>>)
}

pub type Object = Subject;

pub type Predicate = Iri;

pub enum Literal {
    String(String, Tag),
    Numeric(Numeric),
    Boolean(bool)
}

pub enum Tag {
    Lang(String),
    Iri(IriRef)
}

pub enum Numeric {
    Int(u64),
    Decimal(bool, u32, u32), // sign, integer part, decimal part
    Double(bool, u32, u32, i32) // sign, integer part, decimal part, exponent
}
