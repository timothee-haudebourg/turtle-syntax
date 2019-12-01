use std::fmt;
use crate::Loc;
use super::lexer;

pub enum Error {
    Lexer(lexer::Error),
    UnexpectedEos,
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

pub type Result<T> = std::result::Result<T, Loc<Error>>;

impl From<Loc<lexer::Error>> for Loc<Error> {
    fn from(e: Loc<lexer::Error>) -> Loc<Error> {
        let span = e.span();
        Loc::new(Error::Lexer(e.into_inner()), span)
    }
}
