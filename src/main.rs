// src/main.rs

mod lexer;
mod parser;
mod vm;
use std::env;
use std::fs;
use std::io::{self, Write}; // Remove unused Read import
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // Check for debug flag
    let debug = args.iter().any(|arg| arg == "-d");
    
    // Find the input file (first non-flag argument after program name)
    let input_path = args.iter()
        .skip(1) // Skip program name
        .find(|arg| !arg.starts_with("-"))
        .cloned()
        .unwrap_or_else(|| {
            eprintln!("Usage: {} [-d] <inputfile>", args[0]);
            process::exit(1);
        });
    
    // Read from file
    let input = match fs::read_to_string(&input_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", input_path, e);
            process::exit(1);
        }
    };
    
    // Initialize the lexer
    let mut lexer = lexer::Lexer::new(&input);
    
    // Tokenize input
    let tokens = lexer.tokenize();
    if debug {
        println!("Tokens: {:?}", tokens);
    }
    
    // Parse tokens into AST
    match parser::parse_control_flow(tokens) {
        Ok(ast) => {
            if debug {
                println!("Successfully parsed AST");
            }
            
            // Initialize the VM with debug mode if specified
            let mut vm = if debug {
                vm::VM::with_debug(true)
            } else {
                vm::VM::new()
            };
            
            // Run the AST
            match vm.run(ast) {
                Ok(_) => {
                    io::stdout().flush().unwrap(); // Make sure to flush stdout
                },
                Err(e) => {
                    eprintln!("Runtime error: {}", e);
                    process::exit(1);
                }
            }
        },
        Err(e) => {
            eprintln!("Parse error: {}", e);
            process::exit(1);
        }
    }
}