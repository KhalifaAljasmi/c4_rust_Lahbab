// src/lib.rs
#![allow(unused_imports)]  // Add this line to allow unused imports in the exported API

pub mod lexer;
pub mod parser;
pub mod codegen;
pub mod vm;

// Export all needed types directly
pub use lexer::{Token, tokenize, Lexer};
pub use parser::{ASTNode, Expr, Parser, ParseError, parse as parse_program};
pub use codegen::generate_instructions;
pub use vm::{Instruction, VM};
