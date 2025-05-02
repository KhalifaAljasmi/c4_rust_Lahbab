mod lexer;
mod parser;
mod vm;
mod error;
use std::env;
use std::fs;

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: {} <source.c>", args[0]);
        std::process::exit(1);
    }

    let src = fs::read_to_string(&args[1])
        .map_err(|e| Error::InvalidSyntax(format!("I/O error: {}", e)))?;

    let tokens = Lexer::new(&src).tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program()?;

    println!("{:#?}", ast);
    Ok(())
}

