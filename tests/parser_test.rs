use c4_rust_lahbab::{Lexer, Parser, ASTNode, ParseError};

#[test]
fn parse_simple_return() {
    let src = "int main() { return 42; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    assert_eq!(
        ast,
        ASTNode::Program(vec![ASTNode::Function {
            name: "main".into(),
            params: vec![],
            body: vec![ASTNode::Return(Box::new(ASTNode::Number(42)))],
        }])
    );
}

#[test]
fn parse_arithmetic_ops() {
    let src = "int foo() { return 1 + 2 * 3 - 4 / 2; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    // Verify the exact AST structure with proper operator precedence
    assert_eq!(
        ast,
        ASTNode::Program(vec![ASTNode::Function {
            name: "foo".into(),
            params: vec![],
            body: vec![ASTNode::Return(Box::new(ASTNode::BinaryOp {
                op: "-".into(),
                left: Box::new(ASTNode::BinaryOp {
                    op: "+".into(),
                    left: Box::new(ASTNode::Number(1)),
                    right: Box::new(ASTNode::BinaryOp {
                        op: "*".into(),
                        left: Box::new(ASTNode::Number(2)),
                        right: Box::new(ASTNode::Number(3)),
                    }),
                }),
                right: Box::new(ASTNode::BinaryOp {
                    op: "/".into(),
                    left: Box::new(ASTNode::Number(4)),
                    right: Box::new(ASTNode::Number(2)),
                }),
            }))],
        }])
    );
}

#[test]
fn parse_unary_operators() {
    let src = "int bar() { return -x + !y * ~z; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    assert_eq!(
        ast,
        ASTNode::Program(vec![ASTNode::Function {
            name: "bar".into(),
            params: vec![],
            body: vec![ASTNode::Return(Box::new(ASTNode::BinaryOp {
                op: "+".into(),
                left: Box::new(ASTNode::UnaryOp {
                    op: "-".into(),
                    expr: Box::new(ASTNode::Identifier("x".into())),
                }),
                right: Box::new(ASTNode::BinaryOp {
                    op: "*".into(),
                    left: Box::new(ASTNode::UnaryOp {
                        op: "!".into(),
                        expr: Box::new(ASTNode::Identifier("y".into())),
                    }),
                    right: Box::new(ASTNode::UnaryOp {
                        op: "~".into(),
                        expr: Box::new(ASTNode::Identifier("z".into())),
                    }),
                }),
            }))],
        }])
    );
}

#[test]
fn parse_control_flow() {
    let src = r#"
        int test(int x, int i, int j) {
            if (x > 0) {
                return 1;
            } else if (x < 0) {
                return -1;
            } else {
                return 0;
            }
            
            while (i < 10) {
                i = i + 1;
            }
            
            for (j = 0; j < 10; j = j + 1) {
                x = x * 2;
            }
        }
    "#;

    let tokens = Lexer::new(src).tokenize();
    println!("Tokens: {:?}", tokens);

    let mut p = Parser::new(tokens);
    match p.parse_program() {
        Ok(ast) => {
            println!("Parsed AST: {:?}", ast);
            if let ASTNode::Program(fns) = ast {
                if let ASTNode::Function { body, .. } = &fns[0] {
                    assert!(matches!(body[0], ASTNode::If { .. }));
                    assert!(matches!(body[1], ASTNode::While { .. }));
                    assert!(matches!(body[2], ASTNode::For { .. }));
                } else {
                    panic!("Expected a Function node");
                }
            } else {
                panic!("Expected a Program node");
            }
        }
        Err(e) => {
            println!("Error: {}", e);
            panic!("Parsing failed");
        }
    }
}

#[test]
fn parse_function_calls() {
    let src = "int main() { return foo(1, 2 + 3, bar()); }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    assert_eq!(
        ast,
        ASTNode::Program(vec![ASTNode::Function {
            name: "main".into(),
            params: vec![],
            body: vec![ASTNode::Return(Box::new(ASTNode::Call {
                name: "foo".into(),
                args: vec![
                    ASTNode::Number(1),
                    ASTNode::BinaryOp {
                        op: "+".into(),
                        left: Box::new(ASTNode::Number(2)),
                        right: Box::new(ASTNode::Number(3)),
                    },
                    ASTNode::Call {
                        name: "bar".into(),
                        args: vec![],
                    },
                ],
            }))],
        }])
    );
}

#[test]
fn parse_ternary_operator() {
    let src = "int test() { return x > 0 ? 1 : -1; }";
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let ast = p.parse_program().unwrap();

    assert_eq!(
        ast,
        ASTNode::Program(vec![ASTNode::Function {
            name: "test".into(),
            params: vec![],
            body: vec![ASTNode::Return(Box::new(ASTNode::Ternary {
                cond: Box::new(ASTNode::BinaryOp {
                    op: ">".into(),
                    left: Box::new(ASTNode::Identifier("x".into())),
                    right: Box::new(ASTNode::Number(0)),
                }),
                then_expr: Box::new(ASTNode::Number(1)),
                else_expr: Box::new(ASTNode::UnaryOp {
                    op: "-".into(),
                    expr: Box::new(ASTNode::Number(1)),
                }),
            }))],
        }])
    );
}

#[test]
fn error_on_missing_return_semicolon() {
    let src = "int main() { return 5 }";  // missing ;
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let err = p.parse_program().unwrap_err();
    assert_eq!(err.to_string(), "ParseError: Missing semicolon");
}

#[test]
fn error_on_invalid_assignment() {
    let src = "int main() { 42 = x; }";  // can't assign to literal
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let err = p.parse_program().unwrap_err();
    assert_eq!(err.to_string(), "ParseError: Invalid assignment target");
}

#[test]
fn error_on_missing_parenthesis() {
    let src = "int main( { return 0; }";  // missing )
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let err = p.parse_program().unwrap_err();
    assert_eq!(err.to_string(), "ParseError: Missing parenthesis");
}

#[test]
fn error_on_unexpected_token() {
    let src = "int main() { return * 42; }";  // * not allowed as unary op
    let tokens = Lexer::new(src).tokenize();
    let mut p = Parser::new(tokens);
    let err = p.parse_program().unwrap_err();
    assert!(err.to_string().contains("Unexpected token"));
}
