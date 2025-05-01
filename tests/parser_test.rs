use c4_rust_team_alpha::parser;
use c4_rust_team_alpha::compiler::Compiler;
use c4_rust_team_alpha::types::*;

#[test]
fn test_expr_parses_number() {
    let mut c = Compiler::new(false, false).unwrap();
    c.src_code = b"123;".to_vec();
    c.src_code.push(0);
    parser::expr(&mut c, Token::Assign).unwrap();
    assert_eq!(c.e, vec![Opcode::IMM as i64, 123]);
    assert_eq!(c.tk, Some(Token::from(b';' as u32)));
}

#[test]
fn test_expr_parses_modulus() {
    let mut c = Compiler::new(false, false).unwrap();
    c.src_code = b"10 % 3;".to_vec();
    c.src_code.push(0);
    parser::expr(&mut c, Token::Assign).unwrap();
    assert_eq!(c.e, vec![Opcode::IMM as i64, 10, Opcode::PSH as i64, Opcode::IMM as i64, 3, Opcode::MOD as i64]);
    assert_eq!(c.tk, Some(Token::from(b';' as u32)));
}

#[test]
fn test_stmt_parses_if() {
    let mut c = Compiler::new(false, false).unwrap();
    c.src_code = b"if (1) { int x; }".to_vec();
    c.src_code.push(0);
    parser::stmt(&mut c).unwrap();
    assert!(c.e.contains(&Opcode::BZ as i64));
    assert_eq!(c.tk, Some(Token::from(b'}' as u32)));
}
