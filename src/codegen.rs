// src/codegen.rs
#![allow(dead_code)] // suppress warnings for unused ASTNode variants

use crate::vm::Instruction;
use crate::parser::{ASTNode, Expr};
use std::collections::HashMap;

/// Generate a flat Vec<Instruction> from an AST.
pub fn generate_instructions(ast: &ASTNode) -> Vec<Instruction> {
    // If the AST is just a bunch of function definitions, emit a trivial EXIT
    if let ASTNode::Sequence(nodes) = ast {
        if nodes.iter().all(|n| matches!(n, ASTNode::FunctionDef { .. })) {
            return vec![Instruction::IMM(0), Instruction::EXIT];
        }
    }

    let mut instrs = Vec::new();
    let mut symbol_table = HashMap::new();
    let mut next_offset = 0usize;
    let mut patches: Vec<(usize, String)> = Vec::new();

    // reserve an ENT slot for locals
    instrs.push(Instruction::ENT(0));
    generate_instructions_inner(
        ast,
        &mut instrs,
        &mut symbol_table,
        &mut next_offset,
        &mut patches,
    );
    // now fix up the actual size
    instrs[0] = Instruction::ENT(next_offset);

    // back-patch any JSRs we left with dummy targets
    let function_addresses: HashMap<String, usize> = HashMap::new();
    for (idx, name) in patches {
        if let Some(&addr) = function_addresses.get(&name) {
            instrs[idx] = Instruction::JSR(addr);
        } else {
            panic!("Unresolved call to `{}`", name);
        }
    }

    instrs
}

fn generate_instructions_inner(
    node: &ASTNode,
    instrs: &mut Vec<Instruction>,
    symbols: &mut HashMap<String, usize>,
    next_offset: &mut usize,
    patches: &mut Vec<(usize, String)>,
) {
    match node {
        ASTNode::Return(expr) => {
            emit_expr(expr, instrs, symbols, patches);
            instrs.push(Instruction::PSH);
            instrs.push(Instruction::EXIT);
        }

        ASTNode::Print(s) => {
            instrs.push(Instruction::PrintfStr(s.clone()));
        }

        ASTNode::If { condition, then_branch, else_branch } => {
            emit_expr(condition, instrs, symbols, patches);
            let bz_index = instrs.len();
            instrs.push(Instruction::BZ(0)); // placeholder

            generate_instructions_inner(then_branch, instrs, symbols, next_offset, patches);

            if let Some(else_br) = else_branch {
                let jmp_index = instrs.len();
                instrs.push(Instruction::JMP(0)); // placeholder

                let else_start = instrs.len();
                generate_instructions_inner(else_br, instrs, symbols, next_offset, patches);

                let after_else = instrs.len();
                instrs[bz_index] = Instruction::BZ(else_start);
                instrs[jmp_index] = Instruction::JMP(after_else);
            } else {
                let after_then = instrs.len();
                instrs[bz_index] = Instruction::BZ(after_then);
            }
        }

        ASTNode::While { condition, body } => {
            let loop_start = instrs.len();
            emit_expr(condition, instrs, symbols, patches);
            let bz_index = instrs.len();
            instrs.push(Instruction::BZ(0)); // placeholder

            generate_instructions_inner(body, instrs, symbols, next_offset, patches);

            instrs.push(Instruction::JMP(loop_start));
            let loop_end = instrs.len();
            instrs[bz_index] = Instruction::BZ(loop_end);
        }

        ASTNode::Sequence(statements) => {
            for stmt in statements {
                generate_instructions_inner(stmt, instrs, symbols, next_offset, patches);
            }
        }

        ASTNode::Declaration(name, init) => {
            let offset = *next_offset;
            *next_offset += 1;
            symbols.insert(name.clone(), offset);

            instrs.push(Instruction::LEA(offset));
            emit_expr(init, instrs, symbols, patches);
            instrs.push(Instruction::SI);
        }

        ASTNode::Assignment(name, expr) => {
            if let Some(&offset) = symbols.get(name) {
                instrs.push(Instruction::LEA(offset));
                emit_expr(expr, instrs, symbols, patches);
                instrs.push(Instruction::SI);
            } else {
                panic!("Assignment to undeclared variable `{}`", name);
            }
        }

        ASTNode::FunctionDef { name: _, params, body } => {
            // Reset symbol table for each function
            symbols.clear();
            *next_offset = params.len();
            for (i, param) in params.iter().enumerate() {
                symbols.insert(param.clone(), i);
            }
            // Compile function body in-place
            generate_instructions_inner(body, instrs, symbols, next_offset, patches);
        }
    }
}

fn emit_expr(
    expr: &Expr,
    instrs: &mut Vec<Instruction>,
    symbols: &HashMap<String, usize>,
    patches: &mut Vec<(usize, String)>,
) {
    match expr {
        Expr::Number(n) => {
            instrs.push(Instruction::IMM(*n));
        }
        Expr::Add(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::ADD);
        }
        Expr::Sub(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::SUB);
        }
        Expr::Mul(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::MUL);
        }
        Expr::Div(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::DIV);
        }
        Expr::Mod(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::MOD);
        }
        Expr::Equal(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::EQ);
        }
        Expr::Less(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::LT);
        }
        Expr::Greater(a, b) => {
            emit_expr(a, instrs, symbols, patches);
            emit_expr(b, instrs, symbols, patches);
            instrs.push(Instruction::GT);
        }
        Expr::Variable(name) | Expr::Var(name) => {
            if let Some(&offset) = symbols.get(name) {
                instrs.push(Instruction::LEA(offset));
                instrs.push(Instruction::LI);
            } else {
                panic!("Use of undeclared variable `{}`", name);
            }
        }
        Expr::Call(func, args) => {
            for arg in args {
                emit_expr(arg, instrs, symbols, patches);
            }
            let call_site = instrs.len();
            instrs.push(Instruction::JSR(0)); // placeholder
            patches.push((call_site, func.clone()));
        }
    }
}
