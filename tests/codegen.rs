use c4_rust_lahbab::{ASTNode, Expr, generate_instructions, Instruction};

#[test]
fn codegen_simple_return() {
    // AST: { return 7; }
    let ast = ASTNode::Sequence(vec![
        ASTNode::Return(Box::new(Expr::Number(7)))
    ]);
    let instrs = generate_instructions(&ast);
    // ENT(0), IMM(7), PSH, EXIT
    assert_eq!(instrs, vec![
        Instruction::ENT(0),
        Instruction::IMM(7),
        Instruction::PSH,
        Instruction::EXIT,
    ]);
}

#[test]
fn codegen_arithmetic() {
    // AST: { return (2+3)*4; }
    let expr = Expr::Mul(
        Box::new(Expr::Add(
            Box::new(Expr::Number(2)),
            Box::new(Expr::Number(3)),
        )),
        Box::new(Expr::Number(4)),
    );
    let ast = ASTNode::Sequence(vec![ ASTNode::Return(Box::new(expr)) ]);
    let instrs = generate_instructions(&ast);
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
