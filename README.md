# Turtle Syntax

<table><tr>
  <td><a href="https://docs.rs/turtle-syntax">Documentation</a></td>
  <td><a href="https://crates.io/crates/turtle-syntax">Crate informations</a></td>
  <td><a href="https://github.com/timothee-haudebourg/turtle-syntax">Repository</a></td>
</tr></table>

A RDF Turtle parser for Rust based on [`source-span`](https://crates.io/crates/source-span).

## Basic usage

Elements of the Turtle abstract syntax tree (implementing `Parsable`)
are parsed using a `Token` iterator
(a.k.a a lexer). A `Lexer` can be created from a `char` iterator
(here using `utf8_decode::UnsafeDecoder`).
We can then use the `Document::parse` function to parse a Turtle document from
the lexer.

```rust
use turtle_syntax::{Document, Parsable};

// Open the file.
let file = std::fs::File::open(filename)?;

// Create a character stream out of it.
let chars = utf8_decode::UnsafeDecoder::new(file.bytes());

// Create a Turtle token stream (lexer).
let mut lexer = Lexer::new(chars, source_span::DEFAULT_METRICS).peekable();

// Parse the Turtle document.
let doc = Document::parse(&mut lexer, Position::default())?;
```

Every node of the resulting abstract syntax tree is tagged with its position
(span) in the source text using `source_span::Loc`.
This can be used to report eventual syntax error in a human readable way.
```rust
match Document::parse(&mut lexer, Position::default()) {
	Ok(doc) => {
		// do something
	},
	Err(e) => {
		let mut err_fmt = Formatter::new();
		err_fmt.add(e.span(), None, Style::Error);
		let formatted = err_fmt.render(buffer.iter(), buffer.span(), &metrics)?;
		println!("parse error: {}\n{}", e, formatted);
		std::process::exit(1);
	}
}
```
The above code will have the following kind of output when a syntax error is detected:
```text
parse error: invalid iri `http://www.w3.org/TR/rdf-syntax- grammar`

. |
3 | @prefix ex: <http://example.org/stuff/1.0/> .
4 |
5 | <http://www.w3.org/TR/rdf-syntax- grammar>
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
