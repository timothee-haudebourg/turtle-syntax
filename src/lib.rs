//! A RDF Turtle parser for Rust based on [`source-span`](https://crates.io/crates/source-span).
//!
//! ## Basic usage
//!
//! Elements of the Turtle abstract syntax tree (implementing `Parsable`)
//! are parsed using a `Token` iterator
//! (a.k.a a lexer). A `Lexer` can be created from a `char` iterator
//! (here using `utf8_decode::UnsafeDecoder`).
//! We can then use the `Document::parse` function to parse a Turtle document from
//! the lexer.
//!
//! ```rust
//! use turtle_syntax::{Lexer, Document, Parsable};
//! # use std::io::Read;
//! # use source_span::{SourceBuffer,Position,fmt::{Formatter,Style}};
//! # let filename = "examples/sample.ttl";
//!
//! // Open the file.
//! let file = std::fs::File::open(filename).unwrap();
//!
//! // Create a character stream out of it.
//! let chars = utf8_decode::UnsafeDecoder::new(file.bytes());
//!
//! // Create a Turtle token stream (lexer).
//! let mut lexer = Lexer::new(chars, source_span::DEFAULT_METRICS).peekable();
//!
//! // Parse the Turtle document.
//! let doc = Document::parse(&mut lexer, Position::default()).unwrap();
//! ```
//! Every node of the resulting abstract syntax tree is tagged with its position
//! (span) in the source text using [`source_span::Loc`].
//! This can be used to report eventual syntax error in a human readable way.
//! ```rust
//! # use std::io::Read;
//! # use source_span::{SourceBuffer,Position,fmt::{Formatter,Style}};
//! # use turtle_syntax::{Lexer, Document, Parsable};
//! # let file = std::fs::File::open("examples/syntax_error.ttl").unwrap();
//! # let metrics = source_span::DEFAULT_METRICS;
//! # let chars = utf8_decode::UnsafeDecoder::new(file.bytes());
//! # let buffer = SourceBuffer::new(chars, Position::default(), metrics);
//! # let mut lexer = Lexer::new(buffer.iter(), source_span::DEFAULT_METRICS).peekable();
//! match Document::parse(&mut lexer, Position::default()) {
//! 	Ok(doc) => {
//! 		// do something
//! 	},
//! 	Err(e) => {
//! 		let mut err_fmt = Formatter::new();
//! 		err_fmt.add(e.span(), None, Style::Error);
//! 		let formatted = err_fmt.render(buffer.iter(), buffer.span(), &metrics).unwrap();
//! 		println!("parse error: {}\n{}", e, formatted);
//! 	}
//! }
//! ```
//! The above code will have the following kind of output when a syntax error is detected:
//! ```text
//! parse error: invalid iri `http://www.w3.org/TR/rdf-syntax- grammar`
//!
//! . |
//! 3 | @prefix ex: <http://example.org/stuff/1.0/> .
//! 4 |
//! 5 | <http://www.w3.org/TR/rdf-syntax- grammar>
//!   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//! ```

mod error;
mod ast;
pub mod lexer;
mod parse;

pub use error::*;
pub use ast::*;
pub use lexer::Lexer;
pub use parse::*;
