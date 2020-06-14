use std::fs::File;
use std::io::Read;
use utf8_decode::UnsafeDecoder;
use source_span::{
	SourceBuffer,
	Position,
	fmt::{
		Formatter,
		Style
	}
};
use turtle_syntax::{
	Lexer,
	Document,
	Parsable
};

fn main() -> std::io::Result<()> {
	let mut args = std::env::args();
	args.next();

	for filename in args {
		let file = File::open(filename)?;
		let metrics = source_span::DEFAULT_METRICS;
		let chars = UnsafeDecoder::new(file.bytes());
		let buffer = SourceBuffer::new(chars, Position::default(), metrics);
		let mut lexer = Lexer::new(buffer.iter(), metrics).peekable();

		match Document::parse(&mut lexer, Position::default()) {
			Ok(_doc) => {
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
	}

	Ok(())
}
