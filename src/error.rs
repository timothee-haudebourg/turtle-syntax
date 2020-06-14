use std::fmt;
use source_span::Loc;
use super::lexer;

/// Syntax errors.
#[derive(Debug)]
pub enum Error {
	/// Lexing error.
	Lexer(lexer::Error),

	/// Unexpected end of stream.
	UnexpectedEos,

	/// Unexpected token.
	UnexpectedToken(lexer::Token)
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Error::*;
		match self {
			Lexer(e) => write!(f, "{}", e),
			UnexpectedEos => write!(f, "unexpected end of stream"),
			UnexpectedToken(_) => write!(f, "unexpected token")
		}
	}
}

/// Parsing result.
pub type Result<T> = std::result::Result<T, Loc<Error>>;

impl From<lexer::Error> for Error {
	fn from(e: lexer::Error) -> Error {
		Error::Lexer(e)
	}
}
