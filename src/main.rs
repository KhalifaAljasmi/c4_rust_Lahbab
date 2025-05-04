// src/main.rs

mod error;
mod lexer;
mod parser;
mod codegen;
mod vm;

use std::{env, fs, process};
use lexer::Lexer;
use parser::Parser;
use codegen::CodeGen;
use vm::VM;

fn main() {
    let mut args = env::args().skip(1);
    let mut dump_ast = false;
    let mut debug_vm = false;

    // Parse flags
    while let Some(arg) = args.next() {
        if arg == "-s" {
            dump_ast = true;
        } else if arg == "-d" {
            debug_vm = true;
        } else {
            // Treat `arg` as the input file
            let src = fs::read_to_string(&arg).unwrap_or_else(|e| {
                eprintln!("Error reading {}: {}", arg, e);
                process::exit(1);
            });

            // 1) Lex
            let mut lex = Lexer::new(&src);
            let tokens = lex.tokenize();

            // 2) Parse
            let mut parser = Parser::new(tokens);
            let decls = parser.parse().unwrap_or_else(|e| {
                eprintln!("Parse error: {}", e);
                process::exit(1);
            });

            // Dump AST?
            if dump_ast {
                println!("{:#?}", decls);
                return;
            }

            // 3) Codegen
            let mut cg = CodeGen::new();
            let (text, data) = cg.gen(&decls).unwrap_or_else(|e| {
                eprintln!("Codegen error: {}", e);
                process::exit(1);
            });

            // 4) Run in VM
            let mut vm = VM::new(data.len(), 1 << 20);
            vm.debug = debug_vm;
            vm.data = data;
            vm.load_code(text);
            vm.run();

            return;
        }
    }

    eprintln!("Usage: c4_rust_lahbab [-s] [-d] <source.c>");
    process::exit(1);
}
