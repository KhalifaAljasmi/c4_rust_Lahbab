use c4_rust_Lahbab::{Lexer, Parser, ASTNode};

#[test]
fn parse_simple_return() {
    let src = "int main() { return 42; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    let expected = ASTNode::Program(vec![
        ASTNode::Function {
            name: "main".into(),
            body: vec![
                ASTNode::Return(Box::new(ASTNode::Number(42)))
            ],
        }
    ]);

    assert_eq!(ast, expected);
}

#[test]
fn parse_arithmetic_ops() {
    let src = "int foo() { return 1 + 2 * 3 - 4 / 2; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    // We only check that it parsed into a Program with one Function
    // and that that function’s body contains exactly one Return node.
    if let ASTNode::Program(fns) = ast {
        assert_eq!(fns.len(), 1);
        if let ASTNode::Function { body, .. } = &fns[0] {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], ASTNode::Return(_)));
        } else {
            panic!("expected Function");
        }
    } else {
        panic!("expected Program");
    }
}

#[test]
fn error_on_missing_return_semicolon() {
    let src = "int main() { return 5 }";  // missing `;`
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let err = p.parse_program().unwrap_err();
    assert_eq!(err.to_string(), "Unexpected token: Operator(\"}\")");
}
