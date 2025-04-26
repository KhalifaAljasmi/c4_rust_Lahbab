/// Error handling module for compiler-specific errors.

#[derive(Debug)]
pub enum CompilerError {
    LexicalError(String),
    SyntaxError(String),
    RuntimeError(String),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::LexicalError(msg) => write!(f, "Lexical Error: {}", msg),
            CompilerError::SyntaxError(msg) => write!(f, "Syntax Error: {}", msg),
            CompilerError::RuntimeError(msg) => write!(f, "Runtime Error: {}", msg),
        }
    }
}

impl std::error::Error for CompilerError {}
