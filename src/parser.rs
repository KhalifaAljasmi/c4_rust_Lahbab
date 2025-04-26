/// Parser module: Parses tokens into an abstract syntax tree (AST).

use crate::lexer::Token;

#[derive(Debug)]
pub enum ASTNode {
    Declaration(String, i32), // Example: int x = 5;
}

pub fn parse(tokens: &[Token]) -> Vec<ASTNode> {
    let mut ast = Vec::new();
    // TODO: Implement proper parser logic

    // Example placeholder
    if tokens.contains(&Token::Keyword("int".to_string())) {
        ast.push(ASTNode::Declaration("x".to_string(), 5));
    }

    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Token;

    #[test]
    fn test_parse_basic() {
        let tokens = vec![Token::Keyword("int".to_string()), Token::Identifier("x".to_string()), Token::Number(5), Token::EndOfFile];
        let ast = parse(&tokens);
        assert!(!ast.is_empty());
    }
}
