use c4_rust_lahbab::{tokenize, parse_program, ASTNode, Expr};

#[test]
fn parse_return_statement() {
    let src = "int main() { return 42; }";
    let ast = parse_program(&tokenize(src));
    // Expect a top‐level Sequence with one Return(42)
    match ast {
        ASTNode::Sequence(nodes) if nodes.len() == 1 => {
            match &nodes[0] {
                ASTNode::Return(expr) => {
                    assert_eq!(**expr, Expr::Number(42));
                }
                _ => panic!("expected Return node"),
            }
        }
        _ => panic!("expected Sequence"),
    }
}

#[test]
fn parse_if_else() {
    let src = "int main() { if (1) return 2; else return 3; }";
    let ast = parse_program(&tokenize(src));
    // Sequence of one If
    if let ASTNode::Sequence(nodes) = ast {
        if let ASTNode::If { condition, then_branch, else_branch } = &nodes[0] {
            assert_eq!(**condition, Expr::Number(1));
            // then_branch is Return(2)
            if let ASTNode::Return(e) = &**then_branch {
                assert_eq!(**e, Expr::Number(2));
            } else { panic!() }
            // else_branch is Some(Return(3))
            let eb = else_branch.as_ref().unwrap();
            if let ASTNode::Return(e) = &**eb {
                assert_eq!(**e, Expr::Number(3));
            } else { panic!() }
        } else {
            panic!("expected If");
        }
    } else {
        panic!("expected Sequence");
    }
}

#[test]
#[should_panic(expected = "Expected Semicolon")]
fn parse_error_missing_semicolon() {
    let src = "int main() { return 1 }"; // no semicolon
    let _ = parse_program(&tokenize(src));
}
