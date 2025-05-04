// src/codegen.rs

use std::collections::HashMap;
use crate::parser::{Decl, Stmt, Expr, Type, UnaryOp, BinaryOp, EnumVariant};
use crate::error::{Error, Result};
use crate::vm::Opcode;

/// Code generator: AST (`Vec<Decl>`) → C4 bytecode (`text`) + data segment (`data`).
pub struct CodeGen {
    /// Emitted instructions
    pub text: Vec<i64>,
    /// Global/static data
    pub data: Vec<u8>,
    /// Interned string literals: value → offset
    strings: HashMap<String, usize>,
    /// Symbol table: names → data offset (globals/enums) or text offset (functions)
    symbols: HashMap<String, usize>,
    /// Next free byte in `data`
    data_offset: usize,
}

impl CodeGen {
    /// Create a new code generator.
    pub fn new() -> Self {
        CodeGen {
            text: Vec::new(),
            data: Vec::new(),
            strings: HashMap::new(),
            symbols: HashMap::new(),
            data_offset: 0,
        }
    }

    /// Generate bytecode and data for all top-level Decls.
    pub fn gen(&mut self, decls: &[Decl]) -> Result<(Vec<i64>, Vec<u8>)> {
        // 1) Layout enums & globals into `data`
        self.layout_data(decls)?;

        // 2) Emit entry stub: JSR <main_placeholder>; EXIT
        let stub_pos = self.text.len();
        self.emit_op(Opcode::JSR);
        self.emit_i64(0); // placeholder for main's address
        self.emit_op(Opcode::EXIT);

        // 3) Register functions (entry PCs) after stub
        self.register_functions(decls, stub_pos)?;

        // 4) Backpatch main address
        let main_pc = *self.symbols.get("main")
            .ok_or_else(|| Error::CodeGen("`main` not found".into()))?;
        self.text[stub_pos + 1] = main_pc as i64;

        // 5) Emit each function body
        for decl in decls {
            if let Decl::Function { body, .. } = decl {
                // prologue
                self.emit_op(Opcode::ENT);
                self.emit_i64(0); // no locals
                // statements
                for stmt in body {
                    self.gen_stmt(stmt)?;
                }
                // epilogue
                self.emit_op(Opcode::LEV);
            }
        }

        Ok((self.text.clone(), self.data.clone()))
    }

    /// Layout enums and globals in the data segment.
    fn layout_data(&mut self, decls: &[Decl]) -> Result<()> {
        for decl in decls {
            match decl {
                Decl::Enum { variants, .. } => {
                    for EnumVariant { name, value } in variants {
                        self.symbols.insert(name.clone(), *value as usize);
                    }
                }
                Decl::Global { name, ty: _, init } => {
                    // align to 8 bytes
                    let pad = (8 - (self.data_offset % 8)) % 8;
                    if pad > 0 {
                        self.data.resize(self.data_offset + pad, 0);
                        self.data_offset += pad;
                    }
                    // record symbol
                    self.symbols.insert(name.clone(), self.data_offset);
                    // reserve 8 bytes
                    self.data.resize(self.data_offset + 8, 0);
                    if let Some(Expr::Num(n)) = init {
                        self.data[self.data_offset..self.data_offset+8]
                            .copy_from_slice(&n.to_le_bytes());
                    }
                    self.data_offset += 8;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Record each function’s entry point (after the stub).
    fn register_functions(&mut self, decls: &[Decl], _stub_pos: usize) -> Result<()> {
        for decl in decls {
            if let Decl::Function { name, .. } = decl {
                self.symbols.insert(name.clone(), self.text.len());
            }
        }
        Ok(())
    }

    /// Generate code for a statement.
    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.gen_expr(expr)?;
                // pop result
                self.emit_op(Opcode::ADJ);
                self.emit_i64(1);
                Ok(())
            }
            Stmt::Return(Some(expr)) => {
                self.gen_expr(expr)?;
                self.emit_op(Opcode::EXIT);
                Ok(())
            }
            Stmt::Return(None) => {
                self.emit_op(Opcode::EXIT);
                Ok(())
            }
            Stmt::If { cond, then_br, else_br } => {
                self.gen_expr(cond)?;
                self.emit_op(Opcode::BZ);
                let else_pos = self.text.len();
                self.emit_i64(0);
                self.gen_stmt(then_br)?;
                self.emit_op(Opcode::JMP);
                let end_pos = self.text.len();
                self.emit_i64(0);
                // backpatch else
                self.text[else_pos] = self.text.len() as i64;
                if let Some(eb) = else_br {
                    self.gen_stmt(eb)?;
                }
                // backpatch end
                self.text[end_pos] = self.text.len() as i64;
                Ok(())
            }
            Stmt::While { cond, body } => {
                let start = self.text.len() as i64;
                self.gen_expr(cond)?;
                self.emit_op(Opcode::BZ);
                let bz_pos = self.text.len();
                self.emit_i64(0);
                self.gen_stmt(body)?;
                self.emit_op(Opcode::JMP);
                self.emit_i64(start);
                self.text[bz_pos] = self.text.len() as i64;
                Ok(())
            }
            Stmt::For { init, cond, update, body } => {
                if let Some(i) = init { self.gen_expr(i)?; }
                let loop_start = self.text.len() as i64;
                if let Some(c) = cond {
                    self.gen_expr(c)?;
                    self.emit_op(Opcode::BZ);
                    let bz_pos = self.text.len();
                    self.emit_i64(0);
                    self.gen_stmt(body)?;
                    if let Some(u) = update { self.gen_expr(u)?; }
                    self.emit_op(Opcode::JMP);
                    self.emit_i64(loop_start);
                    self.text[bz_pos] = self.text.len() as i64;
                } else {
                    self.gen_stmt(body)?;
                    if let Some(u) = update { self.gen_expr(u)?; }
                    self.emit_op(Opcode::JMP);
                    self.emit_i64(loop_start);
                }
                Ok(())
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s)?;
                }
                Ok(())
            }
            _ => Err(Error::CodeGen(format!("Unhandled stmt {:?}", stmt))),
        }
    }

    /// Generate code for an expression, pushing result into `a`.
    fn gen_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Num(n) => {
                self.emit_op(Opcode::IMM);
                self.emit_i64(*n);
                Ok(())
            }
            Expr::Char(c) => {
                self.emit_op(Opcode::IMM);
                self.emit_i64(*c);
                Ok(())
            }
            Expr::Str(s) => {
                // intern string with NUL terminator
                let off = if let Some(&o) = self.strings.get(s) {
                    o
                } else {
                    let o = self.data_offset;
                    self.data.extend_from_slice(s.as_bytes());
                    self.data.push(0);
                    self.data_offset += s.len() + 1;
                    self.strings.insert(s.clone(), o);
                    o
                };
                self.emit_op(Opcode::IMM);
                self.emit_i64(off as i64);
                Ok(())
            }
            Expr::Ident(name) => {
                let addr = *self.symbols.get(name)
                    .ok_or_else(|| Error::CodeGen(format!("Unknown ident `{}`", name)))?;
                self.emit_op(Opcode::IMM);
                self.emit_i64(addr as i64);
                self.emit_op(Opcode::LI);
                Ok(())
            }
            Expr::SizeOf(ty) => {
                let size = match ty {
                    Type::Char   => 1,
                    Type::Int    => 8,
                    Type::Ptr(_) => 8,
                    Type::Void   => 1,
                };
                self.emit_op(Opcode::IMM);
                self.emit_i64(size);
                Ok(())
            }
            Expr::Cast { expr, .. } => {
                // no-op
                self.gen_expr(expr)?;
                Ok(())
            }
            Expr::Unary { op, expr } => {
                self.gen_expr(expr)?;
                match op {
                    UnaryOp::Minus => {
                        self.emit_op(Opcode::IMM);
                        self.emit_i64(0);
                        self.emit_op(Opcode::SUB);
                    }
                    UnaryOp::Not => {
                        self.emit_op(Opcode::IMM);
                        self.emit_i64(0);
                        self.emit_op(Opcode::EQ);
                    }
                    UnaryOp::BitNot => {
                        self.emit_op(Opcode::IMM);
                        self.emit_i64(-1);
                        self.emit_op(Opcode::XOR);
                    }
                    _ => return Err(Error::CodeGen(format!("Unhandled unary {:?}", op))),
                }
                Ok(())
            }
            Expr::Binary { op, left, right } => {
                self.gen_expr(left)?;
                self.emit_op(Opcode::PSH);
                self.gen_expr(right)?;
                let opc = match op {
                    BinaryOp::Add => Opcode::ADD,
                    BinaryOp::Sub => Opcode::SUB,
                    BinaryOp::Mul => Opcode::MUL,
                    BinaryOp::Div => Opcode::DIV,
                    BinaryOp::Mod => Opcode::MOD,
                    BinaryOp::Eq  => Opcode::EQ,
                    BinaryOp::Ne  => Opcode::NE,
                    BinaryOp::Lt  => Opcode::LT,
                    BinaryOp::Le  => Opcode::LE,
                    BinaryOp::Gt  => Opcode::GT,
                    BinaryOp::Ge  => Opcode::GE,
                    BinaryOp::And => Opcode::AND,
                    BinaryOp::Or  => Opcode::OR,
                    BinaryOp::Xor => Opcode::XOR,
                    BinaryOp::Shl => Opcode::SHL,
                    BinaryOp::Shr => Opcode::SHR,
                    BinaryOp::Assign => {
                        return Err(Error::CodeGen("Assign not supported".into()))
                    }
                    _ => return Err(Error::CodeGen(format!("Unhandled op {:?}", op))),
                };
                self.emit_op(opc);
                Ok(())
            }
            Expr::Conditional { cond, then_, else_ } => {
                self.gen_expr(cond)?;
                self.emit_op(Opcode::BZ);
                let else_pos = self.text.len(); self.emit_i64(0);
                self.gen_expr(then_)?;
                self.emit_op(Opcode::JMP);
                let end_pos = self.text.len(); self.emit_i64(0);
                self.text[else_pos] = self.text.len() as i64;
                self.gen_expr(else_)?;
                self.text[end_pos] = self.text.len() as i64;
                Ok(())
            }
            Expr::Call { func, args } => {
                // push args
                for arg in args {
                    self.gen_expr(arg)?;
                    self.emit_op(Opcode::PSH);
                }
                // dispatch
                if let Expr::Ident(ref name) = **func {
                    if name == "printf" {
                        self.emit_op(Opcode::PRTF);
                    } else {
                        let addr = *self.symbols.get(name)
                            .ok_or_else(|| Error::CodeGen(format!("Unknown fn `{}`", name)))?;
                        self.emit_op(Opcode::JSR);
                        self.emit_i64(addr as i64);
                    }
                } else {
                    return Err(Error::CodeGen(format!("Bad call: {:?}", func)));
                }
                // pop args
                if !args.is_empty() {
                    self.emit_op(Opcode::ADJ);
                    self.emit_i64(args.len() as i64);
                }
                Ok(())
            }
            Expr::Index { base, idx } => {
                self.gen_expr(base)?;
                self.emit_op(Opcode::PSH);
                self.gen_expr(idx)?;
                // scale by 8 (int) or 1 (char)? C4 defaults to 8.
                self.emit_op(Opcode::IMM);
                self.emit_i64(8);
                self.emit_op(Opcode::MUL);
                self.emit_op(Opcode::ADD);
                self.emit_op(Opcode::LI);
                Ok(())
            }
        }
    }

    #[inline]
    fn emit_op(&mut self, op: Opcode) {
        self.text.push(op as i64);
    }

    #[inline]
    fn emit_i64(&mut self, v: i64) {
        self.text.push(v);
    }
}
