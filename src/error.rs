use std::fmt;
use source_span::Loc;
use super::lexer;

/// Syntax errors.
#[derive(Debug)]
pub enum Error<E: std::error::Error> {
	/// Lexing error.
	Lexer(lexer::Error<E>),

	/// Unexpected end of stream.
	UnexpectedEos,

	/// Unexpected token.
	UnexpectedToken(lexer::Token)
}

impl<E: std::error::Error> fmt::Display for Error<E> {
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
pub type Result<T, E> = std::result::Result<T, Loc<Error<E>>>;

impl<E: std::error::Error> From<lexer::Error<E>> for Error<E> {
	fn from(e: lexer::Error<E>) -> Error<E> {
		Error::Lexer(e)
	}
}
