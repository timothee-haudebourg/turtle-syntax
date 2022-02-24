use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use locspan::Loc;
use std::fs::File;
use std::io::Read;
use turtle_syntax::{
	lexing::{Lexer, Utf8Decoded},
	parsing::Parse,
	Document,
};

fn infallible<T>(t: T) -> Result<T, std::convert::Infallible> { Ok(t) }

fn main() -> std::io::Result<()> {
	let mut args = std::env::args();
	args.next();

	let mut files = SimpleFiles::new();

	for filename in args {
		let mut file = File::open(&filename)?;

		let mut buffer = String::new();
		file.read_to_string(&mut buffer)?;
		let file_id = files.add(filename.clone(), buffer);
		let buffer = files.get(file_id).unwrap();

		let chars = Utf8Decoded::new(buffer.source().chars().map(infallible));
		let mut lexer = Lexer::new(file_id, chars.peekable());

		match Document::parse(&mut lexer) {
			Ok(_doc) => {
				// do something
			}
			Err(Loc(e, loc)) => {
				let diagnostic = Diagnostic::error()
					.with_message(format!("parse error: {}", e))
					.with_labels(vec![Label::primary(*loc.file(), loc.span())]);

				let writer = StandardStream::stderr(ColorChoice::Auto);
				let config = codespan_reporting::term::Config::default();
				codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
					.unwrap();
			}
		}
	}

	Ok(())
}
