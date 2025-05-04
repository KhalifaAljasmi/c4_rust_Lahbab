// src/main.rs

mod lexer;
mod parser;
mod codegen;
mod vm;

use std::{env, fs};
use lexer::tokenize;
use parser::parse;
use codegen::generate_instructions;
use vm::VM;

fn main() {
    // collect command‐line args
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <source.c>", args[0]);
        std::process::exit(1);
    }

    // read the source file
    let filename = &args[1];
    let source = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {}", filename, e);
        std::process::exit(1);
    });

    // lex
    let tokens = tokenize(&source);

    // parse into AST
    let ast = parse(&tokens);

    // code‐gen into VM instructions
    let instrs = generate_instructions(&ast);

    // build and run the VM
    let mut vm = VM::new(instrs);
    // vm.enable_trace(); // uncomment to get a step trace
    vm.run();
}
