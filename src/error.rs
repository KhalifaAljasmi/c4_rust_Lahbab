// src/error.rs

use std::fmt;

/// All errors in the C4 compiler pipeline.
#[derive(Debug)]
pub enum Error {
    /// A parsing error with message.
    Syntax(String),
    /// A code‐generation error with message.
    CodeGen(String),
    /// A VM runtime error with message.
    VM(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Syntax(msg)   => write!(f, "Syntax error: {}", msg),
            ParseError::MissingSemicolon { .. } => write!(f, "ParseError: Missing semicolon"),
            Error::CodeGen(msg)  => write!(f, "Codegen error: {}", msg),
            Error::VM(msg)       => write!(f, "Runtime error: {}", msg),
            
        }
    }
}

impl std::error::Error for Error {}

/// Alias for functions that can fail with `Error`.
pub type Result<T> = std::result::Result<T, Error>;
