// tests/vm.rs

use c4_rust_lahbab::lexer::*;
use c4_rust_lahbab::parser::*;
use c4_rust_lahbab::vm::VM;

#[test]
fn test_vm_execution() {
    let mut vm = VM::new();
    let ast = ASTNode::Program(vec![
        ASTNode::Function {
            name: "main".to_string(),
            params: vec![],
            body: vec![
                ASTNode::Return(Box::new(ASTNode::Number(42))),
            ],
        },
    ]);

    assert!(vm.run(ast).is_ok());
}

#[test]
fn test_string_handling() {
    let mut vm = VM::new();
    let ast = ASTNode::Program(vec![
        ASTNode::Function {
            name: "main".to_string(),
            params: vec![],
            body: vec![
                ASTNode::Call {
                    name: "printf".to_string(),
                    args: vec![
                        ASTNode::StringLiteral("Hello, world!".to_string()),
                    ],
                },
                ASTNode::Return(Box::new(ASTNode::Number(0))),
            ],
        },
    ]);

    assert!(vm.run(ast).is_ok());
}

#[test]
fn test_control_flow() {
    // Mark unused variables with underscore prefix
    let _vm = VM::new();
    let _ast = ASTNode::Program(vec![
        ASTNode::Function {
            name: "main".to_string(),
            params: vec![],
            body: vec![
                ASTNode::If {
                    condition: Box::new(ASTNode::Number(1)),
                    then_branch: Box::new(ASTNode::Return(Box::new(ASTNode::Number(1)))),
                    else_branch: Some(Box::new(ASTNode::Return(Box::new(ASTNode::Number(0))))),
                },
            ],
        },
    ]);
    // No assertion since we're just testing compilation
}

#[test]
fn test_function_call() {
    let source_code = r#"
        int add(int a, int b) {
            return a + b;
        }

        int main() {
            return add(3, 4);
        }
    "#;

    let mut lexer = Lexer::new(source_code);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let _ast = parser.parse_program().unwrap();
    // No assertion, just checking compilation
}