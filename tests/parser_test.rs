use c4_rust_lahbab::lexer::{tokenize, Lexer};
use c4_rust_lahbab::parser::{ASTNode, parse as parse_program};

#[test]
fn parse_return_statement() {
    let src = "int main() { return 42; }";
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize();
    let ast = parse_program(tokens).unwrap();
    
    if let ASTNode::Program(nodes) = ast {
        if nodes.len() == 1 {
            if let ASTNode::Function { name, params: _, body } = &nodes[0] {
                assert_eq!(name, "main");
                if let ASTNode::Return(expr) = &body[0] {
                    if let ASTNode::Number(n) = **expr {
                        assert_eq!(n, 42);
                    } else {
                        panic!("expected Number node");
                    }
                } else {
                    panic!("expected Return node");
                }
            } else {
                panic!("expected Function node");
            }
        } else {
            panic!("expected Program with one node");
        }
    } else {
        panic!("expected Program");
    }
}

#[test]
fn test_if_else_parsing() {
    let src = r#"
        int main() {
            if (1) {
                return 2;
            } else {
                return 3;
            }
        }
    "#;

    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize();
    let ast = parse_program(tokens).unwrap();
    
    // Use Program instead of Sequence
    if let ASTNode::Program(nodes) = ast {
        if let ASTNode::Function { name: _, params: _, body } = &nodes[0] {
            if let ASTNode::If { condition, then_branch, else_branch } = &body[0] {
                // Fix dereferencing errors by using the as_ref() pattern
                if let ASTNode::Number(num) = &**condition {
                    assert_eq!(*num, 1);
                } else {
                    panic!("Expected Number node for condition");
                }

                if let ASTNode::Block(stmts) = &**then_branch {
                    if let ASTNode::Return(e) = &stmts[0] {
                        if let ASTNode::Number(num) = &**e {
                            assert_eq!(*num, 2);
                        } else {
                            panic!("Expected Number node for then branch return");
                        }
                    }
                }

                if let Some(else_br) = else_branch {
                    if let ASTNode::Block(stmts) = &**else_br {
                        if let ASTNode::Return(e) = &stmts[0] {
                            if let ASTNode::Number(num) = &**e {
                                assert_eq!(*num, 3);
                            } else {
                                panic!("Expected Number node for else branch return");
                            }
                        }
                    }
                } else {
                    panic!("Expected else branch");
                }
            } else {
                panic!("Expected If node in function body");
            }
        } else {
            panic!("Expected Function node");
        }
    } else {
        panic!("Expected Program node");
    }
}

#[test]
#[should_panic(expected = "MissingSemicolon")]
fn parse_error_missing_semicolon() {
    let src = "int main() { return 1 }"; // no semicolon
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize();
    let _ = parse_program(tokens).unwrap();
}
