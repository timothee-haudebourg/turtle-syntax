mod location;
mod syntax;

pub use location::*;
pub use syntax::*;

// pub enum Error {
//     // ...
// }
//
// impl<'a> TryFrom<&'a Loc<Document>> for grdf::Dataset {
//     type Error = Error;
//
//     fn try_from(doc: &'a Loc<Document>) -> Result<Error, grdf::Dataset> {
//         let mut prefixes = PrefixTable::default();
//
//         for stm in &doc.statements {
//             match stm.as_ref() {
//                 Statement::Directive(Directive::Prefix, iri) => {
//                     // ...
//                 }
//             }
//         }
//     }
// }
