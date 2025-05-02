use c4_rust_Lahbab::{Lexer, Parser, ASTNode, VM, Opcode};

/// Smoke test that the top-level library re-exports work end-to-end.
#[test]
fn smoke_end_to_end_compile_and_run() {
    // 1) Lex + parse
    let src = "int main() { return 7 + 3; }";
    let tokens = Lexer::new(src).tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program().expect("parse_program failed");

    // Should be a Program with one Function
    match ast {
        ASTNode::Program(funcs) => {
            assert_eq!(funcs.len(), 1);
        }
        _ => panic!("expected ASTNode::Program"),
    }

    // 2) Manually assemble bytecode to test VM from root lib
    let code = vec![
        Opcode::IMM as i64, 7,
        Opcode::PSH as i64,
        Opcode::IMM as i64, 3,
        Opcode::ADD as i64,
        Opcode::EXIT as i64,
    ];

    let mut vm = VM::new(128, 128);
    vm.load_code(code);
    vm.run();

    // 7 + 3 = 10
    assert_eq!(vm.a, 10);
}
