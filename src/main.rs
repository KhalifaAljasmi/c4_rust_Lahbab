mod lexer;
mod parser;
mod vm;
use std::env;
use std::fs;

use std::io::{self, Read};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let input = if args.len() > 1 {
        // Read from file
        match fs::read_to_string(&args[1]) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Error reading file '{}': {}", args[1], e);
                process::exit(1);
            }
        }
    } else {
        // Read from stdin
        let mut input = String::new();
        match io::stdin().read_to_string(&mut input) {
            Ok(_) => input,
            Err(e) => {
                eprintln!("Error reading from stdin: {}", e);
                process::exit(1);
            }
        }
    };
    
    // Initialize the lexer
    let mut lexer = lexer::Lexer::new(&input);
    
    // Tokenize input
    let tokens = lexer.tokenize();
    
    // Parse tokens into AST
    match parser::parse_control_flow(tokens) {
        Ok(ast) => {
            // Initialize the VM
            let mut vm = vm::VM::new();
            
            // Run the AST
            match vm.run(ast) {
                Ok(_) => {},
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