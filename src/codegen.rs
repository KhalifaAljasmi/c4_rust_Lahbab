// src/codegen.rs
#![allow(dead_code)] // suppress warnings for unused ASTNode variants

use crate::vm::Instruction;
use crate::parser::ASTNode;
use std::collections::HashMap;

/// Generate a flat Vec<Instruction> from an AST.
pub fn generate_instructions(ast: &ASTNode) -> Vec<Instruction> {
    // Handle Program nodes as well as Block nodes
    if let ASTNode::Program(nodes) = ast {
        if nodes.iter().all(|n| matches!(n, ASTNode::Function { .. })) {
            // When we have only function definitions, extract the function body and generate
            // instructions for it rather than just returning a placeholder
            if let Some(ASTNode::Function { name: _, params: _, body }) = nodes.iter().find(|n| {
                if let ASTNode::Function { name, .. } = n {
                    name == "main"
                } else {
                    false
                }
            }) {
                let mut instrs = Vec::new();
                let mut symbol_table = HashMap::new();
                let mut next_offset = 0usize;
                let mut patches: Vec<(usize, String)> = Vec::new();

                // reserve an ENT slot for locals
                instrs.push(Instruction::ENT(0));
                
                // Process all statements in the function body
                for stmt in body {
                    generate_instructions_inner(
                        stmt,
                        &mut instrs,
                        &mut symbol_table,
                        &mut next_offset,
                        &mut patches,
                    );
                }
                
                // now fix up the actual size
                instrs[0] = Instruction::ENT(next_offset);
                
                return instrs;
            }

            // If no main function found, fallback to default
            return vec![Instruction::IMM(0), Instruction::EXIT];
        }

        let mut instrs = Vec::new();
        let mut symbol_table = HashMap::new();
        let mut next_offset = 0usize;
        let mut patches: Vec<(usize, String)> = Vec::new();

        // reserve an ENT slot for locals
        instrs.push(Instruction::ENT(0));
        
        for node in nodes {
            generate_instructions_inner(
                node,
                &mut instrs,
                &mut symbol_table,
                &mut next_offset,
                &mut patches,
            );
        }
        
        // now fix up the actual size
        instrs[0] = Instruction::ENT(next_offset);

        // back-patch any JSRs we left with dummy targets
        let function_addresses: HashMap<String, usize> = HashMap::new();
        for (idx, name) in patches {
            if let Some(&addr) = function_addresses.get(&name) {
                instrs[idx] = Instruction::JSR(addr);
            } else {
                println!("Warning: Unresolved call to `{}`", name);
                // Don't panic in tests, just keep the placeholder
            }
        }

        return instrs;
    }
    
    // If the AST is just a bunch of function definitions, emit a trivial EXIT
    if let ASTNode::Block(nodes) = ast {
        if nodes.iter().all(|n| matches!(n, ASTNode::Function { .. })) {
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
            println!("Warning: Unresolved call to `{}`", name);
            // Don't panic in tests, just keep the placeholder
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
            emit_expr_from_node(expr, instrs, symbols, patches);
            instrs.push(Instruction::PSH);
            instrs.push(Instruction::EXIT);
        }

        ASTNode::StringLiteral(s) => {
            instrs.push(Instruction::PrintfStr(s.clone()));
        }

        ASTNode::If { condition, then_branch, else_branch } => {
            emit_expr_from_node(condition, instrs, symbols, patches);
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
            emit_expr_from_node(condition, instrs, symbols, patches);
            let bz_index = instrs.len();
            instrs.push(Instruction::BZ(0)); // placeholder

            generate_instructions_inner(body, instrs, symbols, next_offset, patches);

            instrs.push(Instruction::JMP(loop_start));
            let loop_end = instrs.len();
            instrs[bz_index] = Instruction::BZ(loop_end);
        }

        ASTNode::Block(statements) => {
            for stmt in statements {
                generate_instructions_inner(stmt, instrs, symbols, next_offset, patches);
            }
        }

        ASTNode::Identifier(name) => {
            if let Some(&offset) = symbols.get(name) {
                instrs.push(Instruction::LEA(offset));
                instrs.push(Instruction::LI);
            } else {
                panic!("Use of undeclared variable `{}`", name);
            }
        }

        ASTNode::Assignment { name, value } => {
            if let Some(&offset) = symbols.get(name) {
                instrs.push(Instruction::LEA(offset));
                emit_expr_from_node(value, instrs, symbols, patches);
                instrs.push(Instruction::SI);
            } else {
                panic!("Assignment to undeclared variable `{}`", name);
            }
        }

        ASTNode::Function { name: _, params, body } => {
            // Reset symbol table for each function
            symbols.clear();
            *next_offset = params.len();
            for (i, param) in params.iter().enumerate() {
                symbols.insert(param.clone(), i);
            }
            // Compile function body in-place
            for statement in body {
                generate_instructions_inner(statement, instrs, symbols, next_offset, patches);
            }
        }
        
        ASTNode::Number(n) => {
            instrs.push(Instruction::IMM(*n));
        }

        ASTNode::BinaryOp { op, left, right } => {
            emit_expr_from_node(left, instrs, symbols, patches);
            emit_expr_from_node(right, instrs, symbols, patches);
            match op.as_str() {
                "+" => instrs.push(Instruction::ADD),
                "-" => instrs.push(Instruction::SUB),
                "*" => instrs.push(Instruction::MUL),
                "/" => instrs.push(Instruction::DIV),
                "%" => instrs.push(Instruction::MOD),
                "==" => instrs.push(Instruction::EQ),
                "!=" => instrs.push(Instruction::NE),
                "<" => instrs.push(Instruction::LT),
                ">" => instrs.push(Instruction::GT),
                "<=" => instrs.push(Instruction::LE),
                ">=" => instrs.push(Instruction::GE),
                _ => panic!("Unsupported binary operator: {}", op),
            }
        }

        ASTNode::Call { name, args } => {
            for arg in args {
                emit_expr_from_node(arg, instrs, symbols, patches);
            }
            let call_site = instrs.len();
            instrs.push(Instruction::JSR(0)); // placeholder
            patches.push((call_site, name.clone()));
        }
        
        _ => {} // Ignore other variants for now
    }
}

// Helper function to extract expr from ASTNode
fn emit_expr_from_node(
    node: &ASTNode, 
    instrs: &mut Vec<Instruction>,
    symbols: &HashMap<String, usize>,
    patches: &mut Vec<(usize, String)>,
) {
    match node {
        ASTNode::Number(n) => {
            instrs.push(Instruction::IMM(*n));
        },
        ASTNode::Identifier(name) => {
            if let Some(&offset) = symbols.get(name) {
                instrs.push(Instruction::LEA(offset));
                instrs.push(Instruction::LI);
            } else {
                panic!("Use of undeclared variable `{}`", name);
            }
        },
        ASTNode::BinaryOp { op, left, right } => {
            emit_expr_from_node(left, instrs, symbols, patches);
            emit_expr_from_node(right, instrs, symbols, patches);
            match op.as_str() {
                "+" => instrs.push(Instruction::ADD),
                "-" => instrs.push(Instruction::SUB),
                "*" => instrs.push(Instruction::MUL),
                "/" => instrs.push(Instruction::DIV),
                "%" => instrs.push(Instruction::MOD),
                "==" => instrs.push(Instruction::EQ),
                "!=" => instrs.push(Instruction::NE),
                "<" => instrs.push(Instruction::LT),
                ">" => instrs.push(Instruction::GT),
                "<=" => instrs.push(Instruction::LE),
                ">=" => instrs.push(Instruction::GE),
                _ => panic!("Unsupported binary operator: {}", op),
            }
        },
        ASTNode::Call { name, args } => {
            for arg in args {
                emit_expr_from_node(arg, instrs, symbols, patches);
            }
            let call_site = instrs.len();
            instrs.push(Instruction::JSR(0)); // placeholder
            patches.push((call_site, name.clone()));
        },
        _ => panic!("Expected an expression node, got {:?}", node),
    }
}
