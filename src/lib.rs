// src/lib.rs

pub mod error;
pub mod lexer;
pub mod parser;
pub mod codegen;
pub mod vm;

pub use error::{Error, Result};
pub use lexer::{Lexer, Token};
pub use parser::{Parser, Decl, Type, Stmt, Expr, UnaryOp, BinaryOp, EnumVariant};
pub use codegen::CodeGen;
pub use vm::{VM, Opcode};
