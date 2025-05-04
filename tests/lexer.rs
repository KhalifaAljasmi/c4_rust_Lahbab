use c4_rust_lahbab::lexer::{Lexer, Token};

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("int return while if");
    assert_eq!(lexer.next_token(), Token::Keyword("int".to_string()));
    assert_eq!(lexer.next_token(), Token::Keyword("return".to_string()));
    assert_eq!(lexer.next_token(), Token::Keyword("while".to_string()));
    assert_eq!(lexer.next_token(), Token::Keyword("if".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_identifiers() {
    let mut lexer = Lexer::new("variableName another_var");
    assert_eq!(lexer.next_token(), Token::Id("variableName".to_string()));
    assert_eq!(lexer.next_token(), Token::Id("another_var".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_numbers_decimal() {
    let mut lexer = Lexer::new("123 4567");
    assert_eq!(lexer.next_token(), Token::Num(123));
    assert_eq!(lexer.next_token(), Token::Num(4567));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_numbers_hex_and_octal() {
    let mut lexer = Lexer::new("0x1F 075");
    assert_eq!(lexer.next_token(), Token::Num(31)); // 0x1F = 31
    assert_eq!(lexer.next_token(), Token::Num(61)); // 075 = octal = 61
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_string_and_char_literals() {
    let mut lexer = Lexer::new("'a' \"hello\"");
    assert_eq!(lexer.next_token(), Token::CharLiteral('a'));
    assert_eq!(lexer.next_token(), Token::StringLiteral("hello".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_basic_operators() {
    let mut lexer = Lexer::new("+ - * / = ; ( ) { }");
    assert_eq!(lexer.next_token(), Token::Operator("+".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("-".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("*".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("/".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("=".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator(";".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("(".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator(")".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("{".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("}".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_multi_char_operators() {
    let mut lexer = Lexer::new("== != <= >= && || << >>");
    assert_eq!(lexer.next_token(), Token::Operator("==".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("!=".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("<=".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator(">=".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("&&".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("||".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator("<<".to_string()));
    assert_eq!(lexer.next_token(), Token::Operator(">>".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_skip_comments() {
    let mut lexer = Lexer::new("// this is a comment\nint");
    assert_eq!(lexer.next_token(), Token::Keyword("int".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}
#[test]
fn test_unknown_character() {
    let mut lexer = Lexer::new("@");
    assert_eq!(lexer.next_token(), Token::Unknown('@'));
    assert_eq!(lexer.next_token(), Token::EOF);
}
#[test]
fn test_tokenize_full_line() {
    let mut lexer = Lexer::new("int main() { return 42; }");
    let tokens = lexer.tokenize();
    let expected = vec![
        Token::Keyword("int".to_string()),
        Token::Id("main".to_string()),                   
        Token::Operator("(".to_string()),
        Token::Operator(")".to_string()),
        Token::Operator("{".to_string()),
        Token::Keyword("return".to_string()),
        Token::Num(42),
        Token::Operator(";".to_string()),
        Token::Operator("}".to_string()),
        Token::EOF,
    ];
    assert_eq!(tokens, expected);
}



