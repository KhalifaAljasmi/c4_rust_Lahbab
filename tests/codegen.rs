use c4_rust_lahbab::codegen::generate_instructions;
use c4_rust_lahbab::parser::ASTNode;
use c4_rust_lahbab::vm::Instruction;

#[test]
fn test_return_stmt() {
    // Update to use Program instead of Sequence
    let ast = ASTNode::Program(vec![
        ASTNode::Function {
            name: "main".to_string(),
            params: vec![],
            body: vec![
                ASTNode::Return(Box::new(ASTNode::Number(7)))
            ],
        }
    ]);

    let instrs = generate_instructions(&ast);
    
    // Debug output to help diagnose the issue
    println!("Generated instructions: {:?}", instrs);
    
    // Verify it contains IMM 7 and EXIT
    assert!(instrs.iter().any(|i| match i {
        Instruction::IMM(7) => true,
        _ => false
    }));
    assert!(instrs.iter().any(|i| match i {
        Instruction::EXIT => true,
        _ => false
    }));
}

#[test]
fn codegen_arithmetic() {
    // Create an expression using ASTNode directly instead of Expr
    let ast = ASTNode::Program(vec![
        ASTNode::Return(Box::new(
            ASTNode::BinaryOp {
                op: "*".to_string(),
                left: Box::new(ASTNode::BinaryOp {
                    op: "+".to_string(),
                    left: Box::new(ASTNode::Number(2)),
                    right: Box::new(ASTNode::Number(3)),
                }),
                right: Box::new(ASTNode::Number(4)),
            }
        ))
    ]);
    
    let instrs = generate_instructions(&ast);
    
    // Debug output to help diagnose the issue
    println!("Generated instructions: {:?}", instrs);
    
    // ENT(0), IMM2, IMM3, ADD, IMM4, MUL, PSH, EXIT
    assert_eq!(instrs, vec![
        Instruction::ENT(0),
        Instruction::IMM(2),
        Instruction::IMM(3),
        Instruction::ADD,
        Instruction::IMM(4),
        Instruction::MUL,
        Instruction::PSH,
        Instruction::EXIT,
    ]);
}
