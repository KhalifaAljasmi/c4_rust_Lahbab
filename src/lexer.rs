/// Lexer module: Responsible for tokenizing input C code.

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Number(i32),
    Operator(char),
    Separator(char),
    EndOfFile,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    // TODO: Implement proper lexer logic here

    // Example placeholder: very basic
    if input.contains("int") {
        tokens.push(Token::Keyword("int".to_string()));
    }

    tokens.push(Token::EndOfFile);
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_basic() {
        let input = "int x = 5;";
        let tokens = tokenize(input);
        assert!(tokens.contains(&Token::Keyword("int".to_string())));
    }
}
