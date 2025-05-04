// src/lib.rs

pub mod lexer;
pub mod parser;
pub mod codegen;
pub mod vm;

pub use lexer::Token;
pub use lexer::tokenize;
pub use parser::{ASTNode, Expr, parse as parse_program};
pub use codegen::generate_instructions;
pub use vm::{Instruction, VM};
