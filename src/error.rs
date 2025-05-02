// Test push after rename

//! Unified error type for lexer, parser, and VM.

use std::fmt;

/// All errors in the compiler pipeline.
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(String),
    UnexpectedEOF,
    InvalidSyntax(String),
    VMError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedToken(t) => write!(f, "Unexpected token: {}", t),
            Error::UnexpectedEOF     => write!(f, "Unexpected end of input"),
            Error::InvalidSyntax(s)  => write!(f, "Invalid syntax: {}", s),
            Error::VMError(s)        => write!(f, "VM error: {}", s),
        }
    }
}

impl std::error::Error for Error {}

/// Handy alias used throughout.
pub type Result<T> = std::result::Result<T, Error>;
