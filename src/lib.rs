//! Turtle is a textual syntax for RDF that allows an RDF graph to be completely
//! written in a compact and natural text form, with abbreviations for common
//! usage patterns and datatypes. This library provides a Turtle parser for Rust that keeps tracks of code mapping metadata for each syntax node using the [`locspan`](https://crates.io/crates/locspan) library.
//!
//! ## Basic usage
//!
//! Elements of the Turtle abstract syntax tree (implementing `Parsable`)
//! are parsed using a `Token` iterator (a.k.a a lexer).
//! A `Lexer` can be created from a `char` iterator.
//! We can then use the `Document::parse` function to parse a Turtle document
//! from the lexer.
//! In the following example, we use the excellent [`codespan_reporting`](https://crates.io/crates/codespan-reporting) crate
//! to report errors.
//!
//! ```rust
//! use codespan_reporting::diagnostic::{Diagnostic, Label};
//! use codespan_reporting::files::SimpleFiles;
//! use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
//! use locspan::Loc;
//! use std::fs::File;
//! use std::io::Read;
//! use turtle_syntax::{
//!   lexing::{Lexer, Utf8Decoded},
//!   parsing::Parsable,
//!   Document,
//! };
//!
//! fn infallible<T>(t: T) -> Result<T, std::convert::Infallible> { Ok(t) }
//!
//! fn main() -> std::io::Result<()> {
//!   let mut args = std::env::args();
//!   args.next();
//!
//!   let mut files = SimpleFiles::new();
//!
//!   for filename in args {
//!     let mut file = File::open(&filename)?;
//!
//!     let mut buffer = String::new();
//!     file.read_to_string(&mut buffer)?;
//!     let file_id = files.add(filename.clone(), buffer);
//!     let buffer = files.get(file_id).unwrap();
//!
//!     let chars = Utf8Decoded::new(buffer.source().chars().map(infallible));
//!     let mut lexer = Lexer::new(file_id, chars.peekable());
//!
//!     match Document::parse(&mut lexer) {
//!       Ok(Loc(doc, _)) => {
//!         for Loc(statement, loc) in doc.statements {
//!           // do something...
//!         }
//!       }
//!       Err(Loc(e, loc)) => {
//!         let diagnostic = Diagnostic::error()
//!           .with_message(format!("parse error: {}", e))
//!           .with_labels(vec![Label::primary(*loc.file(), loc.span())]);
//!
//!         let writer = StandardStream::stderr(ColorChoice::Auto);
//!         let config = codespan_reporting::term::Config::default();
//!         codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
//!           .unwrap();
//!       }
//!     }
//!   }
//!
//!   Ok(())
//! }
//! ```
//!
//! The above code will have the following kind of output when a syntax error is
//! detected:
//! ```text
//! error: parse error: unexpected character ` `
//!   ┌─ examples/syntax_error.ttl:5:34
//!   │
//! 5 │ <http://www.w3.org/TR/rdf-syntax- grammar>
//!   │                                  ^
//! ```
mod ast;
pub mod lexing;
pub mod parsing;

pub use ast::*;
