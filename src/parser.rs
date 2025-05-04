// src/parser.rs

use crate::lexer::Token;
use crate::error::{Error, Result};
use std::collections::VecDeque;

/// C4 types: char, int, void, and pointers.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    Int,
    Void,
    Ptr(Box<Type>),
}

/// One variant in an enum declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub value: i64,
}

/// Top‐level declarations.
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Enum {
        name: Option<String>,
        variants: Vec<EnumVariant>,
    },
    Global {
        name: String,
        ty: Type,
        init: Option<Expr>,
    },
    Function {
        name: String,
        ret_ty: Type,
        params: Vec<(String, Type)>,
        body: Vec<Stmt>,
    },
}

/// Statements, including local declarations.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Expr(Expr),
    Return(Option<Expr>),
    If {
        cond: Expr,
        then_br: Box<Stmt>,
        else_br: Option<Box<Stmt>>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    For {
        init: Option<Expr>,
        cond: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(i64),
    Char(i64),
    Str(String),
    Ident(String),
    SizeOf(Type),
    Cast { to: Type, expr: Box<Expr> },
    Unary { op: UnaryOp, expr: Box<Expr> },
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Conditional { cond: Box<Expr>, then_: Box<Expr>, else_: Box<Expr> },
    Call { func: Box<Expr>, args: Vec<Expr> },
    Index { base: Box<Expr>, idx: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Plus, Minus, Not, BitNot, Deref, Addr,
    PreInc, PreDec, PostInc, PostDec,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Assign, Lor, Lan, Or, Xor, And,
    Eq, Ne, Lt, Gt, Le, Ge,
    Shl, Shr, Add, Sub, Mul, Div, Mod,
}

/// Parser with a small queue for comma‐separated globals.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    pending_globals: VecDeque<Decl>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0, pending_globals: VecDeque::new() }
    }

    /// Parse all top‐level declarations until EOF.
    pub fn parse(&mut self) -> Result<Vec<Decl>> {
        let mut decls = Vec::new();
        while self.peek() != Token::EOF {
            if let Some(g) = self.pending_globals.pop_front() {
                decls.push(g);
            } else {
                decls.push(self.parse_decl()?);
            }
        }
        Ok(decls)
    }

    /// Parse one top‐level decl: enum, globals (comma‐list), or function.
    fn parse_decl(&mut self) -> Result<Decl> {
        if let Token::Keyword(ref k) = self.peek() {
            // enum
            if k == "enum" {
                return self.parse_enum_decl();
            }
            // int/char/void globals or functions
            if k == "int" || k == "char" || k == "void" {
                let base_ty = self.parse_type()?;
                // pointer stars before name
                let mut ty0 = base_ty.clone();
                while self.peek() == Token::Operator("*".into()) {
                    self.advance();
                    ty0 = Type::Ptr(Box::new(ty0));
                }
                let name = self.consume_ident()?;

                // function?
                if self.peek() == Token::Operator("(".into()) {
                    return self.parse_function_decl(name, ty0);
                }

                // otherwise globals comma‐list
                let mut globals = Vec::new();
                globals.push(Decl::Global { name: name.clone(), ty: ty0.clone(), init: None });

                // optional init for first
                if self.peek() == Token::Operator("=".into()) {
                    self.advance();
                    let e = self.parse_expr()?;
                    if let Decl::Global { ref mut init, .. } = globals[0] { *init = Some(e); }
                }

                // further comma-separated
                while self.peek() == Token::Operator(",".into()) {
                    self.advance();
                    let mut ty = base_ty.clone();
                    while self.peek() == Token::Operator("*".into()) {
                        self.advance();
                        ty = Type::Ptr(Box::new(ty));
                    }
                    let nm = self.consume_ident()?;
                    let mut init = None;
                    if self.peek() == Token::Operator("=".into()) {
                        self.advance();
                        init = Some(self.parse_expr()?);
                    }
                    globals.push(Decl::Global { name: nm, ty, init });
                }

                self.expect_op(";")?;
                let first = globals.remove(0);
                for g in globals.into_iter().rev() {
                    self.pending_globals.push_front(g);
                }
                return Ok(first);
            }
        }
        Err(Error::Syntax(format!("Unexpected token {:?}", self.peek())))
    }

    fn parse_enum_decl(&mut self) -> Result<Decl> {
        self.consume_keyword("enum")?;
        let name = if let Token::Id(_) = self.peek() {
            Some(self.consume_ident()?)
        } else {
            None
        };
        self.expect_op("{")?;
        let mut variants = Vec::new();
        let mut val = 0;
        loop {
            let vname = self.consume_ident()?;
            if self.peek() == Token::Operator("=".into()) {
                self.advance();
                if let Expr::Num(n) = self.parse_expr()? { val = n; }
            }
            variants.push(EnumVariant { name: vname, value: val });
            val += 1;
            if self.peek() == Token::Operator(",".into()) {
                self.advance();
            } else {
                break;
            }
        }
        self.expect_op("}")?;
        self.expect_op(";")?;
        Ok(Decl::Enum { name, variants })
    }

    fn parse_function_decl(&mut self, name: String, ret_ty: Type) -> Result<Decl> {
        self.expect_op("(")?;
        let mut params = Vec::new();
        while self.peek() != Token::Operator(")".into()) {
            // parse parameter type
            let base = self.parse_type()?;
            let mut pty = base.clone();
            while self.peek() == Token::Operator("*".into()) {
                self.advance();
                pty = Type::Ptr(Box::new(pty));
            }
            // then the identifier
            let pid = self.consume_ident()?;
            params.push((pid, pty));
            if self.peek() == Token::Operator(",".into()) {
                self.advance();
            }
        }
        self.expect_op(")")?;
        let body = self.parse_block_stmt()?;
        if let Stmt::Block(stmts) = body {
            Ok(Decl::Function { name, ret_ty, params, body: stmts })
        } else {
            unreachable!()
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt> {
        self.expect_op("{")?;
        let mut stmts = Vec::new();
        while self.peek() != Token::Operator("}".into()) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect_op("}")?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        // local declarations
        if let Token::Keyword(ref k) = self.peek() {
            if k == "int" || k == "char" || k == "void" {
                let base = self.parse_type()?;
                let mut locals = Vec::new();
                loop {
                    let mut ty = base.clone();
                    while self.peek() == Token::Operator("*".into()) {
                        self.advance();
                        ty = Type::Ptr(Box::new(ty));
                    }
                    let nm = self.consume_ident()?;
                    locals.push(Decl::Global { name: nm, ty, init: None });
                    if self.peek() == Token::Operator(",".into()) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                self.expect_op(";")?;
                return Ok(Stmt::Decl(locals.remove(0)));
            }
        }

        match self.peek() {
            Token::Keyword(ref k) if k == "return" => {
                self.advance();
                let e = if self.peek() != Token::Operator(";".into()) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect_op(";")?;
                Ok(Stmt::Return(e))
            }
            Token::Keyword(ref k) if k == "if" => {
                self.advance(); self.expect_op("(")?;
                let cond = self.parse_expr()?; self.expect_op(")")?;
                let then_br = Box::new(self.parse_stmt()?);
                let else_br = if self.peek() == Token::Keyword("else".into()) {
                    self.advance();
                    Some(Box::new(self.parse_stmt()?))
                } else {
                    None
                };
                Ok(Stmt::If { cond, then_br, else_br })
            }
            Token::Keyword(ref k) if k == "while" => {
                self.advance(); self.expect_op("(")?;
                let cond = self.parse_expr()?; self.expect_op(")")?;
                Ok(Stmt::While { cond, body: Box::new(self.parse_stmt()?) })
            }
            Token::Keyword(ref k) if k == "for" => {
                self.advance(); self.expect_op("(")?;
                let init = if self.peek() != Token::Operator(";".into()) {
                    Some(self.parse_expr()?)
                } else { None };
                self.expect_op(";")?;
                let cond = if self.peek() != Token::Operator(";".into()) {
                    Some(self.parse_expr()?)
                } else { None };
                self.expect_op(";")?;
                let update = if self.peek() != Token::Operator(")".into()) {
                    Some(self.parse_expr()?)
                } else { None };
                self.expect_op(")")?;
                Ok(Stmt::For {
                    init, cond, update,
                    body: Box::new(self.parse_stmt()?),
                })
            }
            Token::Operator(ref s) if s == "{" => self.parse_block_stmt(),
            _ => {
                let e = self.parse_expr()?;
                self.expect_op(";")?;
                Ok(Stmt::Expr(e))
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> { self.parse_conditional() }

    fn parse_conditional(&mut self) -> Result<Expr> {
        let mut e = self.parse_binary(0)?;
        if self.peek() == Token::Operator("?".into()) {
            self.advance();
            let t = Box::new(self.parse_expr()?);
            self.expect_op(":")?;
            let f = Box::new(self.parse_expr()?);
            e = Expr::Conditional { cond: Box::new(e), then_: t, else_: f };
        }
        Ok(e)
    }

    fn parse_binary(&mut self, prec: usize) -> Result<Expr> {
        const OPS: &[(&[&str], fn() -> BinaryOp)] = &[
            (&["="],   || BinaryOp::Assign),
            (&["||"],  || BinaryOp::Lor),
            (&["&&"],  || BinaryOp::Lan),
            (&["|"],   || BinaryOp::Or),
            (&["^"],   || BinaryOp::Xor),
            (&["&"],   || BinaryOp::And),
            (&["==","!="], || BinaryOp::Eq),
            (&["<",">","<=",">="], || BinaryOp::Lt),
            (&["<<",">>"], || BinaryOp::Shl),
            (&["+","-"], || BinaryOp::Add),
            (&["*","/","%"], || BinaryOp::Mul),
        ];
        if prec == OPS.len() {
            return self.parse_unary();
        }
        let mut lhs = self.parse_binary(prec+1)?;
        loop {
            let mut found = None;
            if let Token::Operator(ref s) = self.peek() {
                for &(syms, ctor) in &OPS[prec..=prec] {
                    if syms.contains(&s.as_str()) {
                        found = Some(ctor());
                        break;
                    }
                }
            }
            if let Some(op) = found {
                self.advance();
                let rhs = self.parse_binary(prec+1)?;
                lhs = Expr::Binary { op, left: Box::new(lhs), right: Box::new(rhs) };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if let Token::Operator(ref s) = self.peek() {
            // prefix ++/--
            if s == "++" {
                self.advance();
                let ex = Box::new(self.parse_unary()?);
                return Ok(Expr::Unary { op: UnaryOp::PreInc, expr: ex });
            }
            if s == "--" {
                self.advance();
                let ex = Box::new(self.parse_unary()?);
                return Ok(Expr::Unary { op: UnaryOp::PreDec, expr: ex });
            }
            // other unary
            if let Some(op) = match s.as_str() {
                "+" => Some(UnaryOp::Plus),
                "-" => Some(UnaryOp::Minus),
                "!" => Some(UnaryOp::Not),
                "~" => Some(UnaryOp::BitNot),
                "*" => Some(UnaryOp::Deref),
                "&" => Some(UnaryOp::Addr),
                _ => None,
            } {
                self.advance();
                let ex = Box::new(self.parse_unary()?);
                return Ok(Expr::Unary { op, expr: ex });
            }
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut e = match self.peek() {
            Token::Num(n)               => { self.advance(); Expr::Num(n) },
            Token::CharLiteral(c)       => { self.advance(); Expr::Char(c as i64) },
            Token::StringLiteral(ref s) => { let s2 = s.clone(); self.advance(); Expr::Str(s2) },
            Token::Id(ref id)           => { let i = id.clone(); self.advance(); Expr::Ident(i) },
            Token::Keyword(ref k) if k == "sizeof" => {
                self.advance(); self.expect_op("(")?;
                let t = self.parse_type()?; self.expect_op(")")?;
                Expr::SizeOf(t)
            }
            Token::Operator(ref s) if s == "(" => {
                self.advance();
                if let Token::Keyword(ref k) = self.peek() {
                    if k=="char"||k=="int"||k=="void" {
                        let mut ty = self.parse_type()?;
                        while self.peek()==Token::Operator("*".into()) {
                            self.advance(); ty = Type::Ptr(Box::new(ty));
                        }
                        self.expect_op(")")?;
                        let sub = self.parse_unary()?;
                        Expr::Cast { to: ty, expr: Box::new(sub) }
                    } else {
                        let ex = self.parse_expr()?; self.expect_op(")")?; ex
                    }
                } else {
                    let ex = self.parse_expr()?; self.expect_op(")")?; ex
                }
            }
            _ => return Err(Error::Syntax(format!("Unexpected primary {:?}", self.peek()))),
        };

        loop {
            if self.peek()==Token::Operator("(".into()) {
                self.advance();
                let mut args = Vec::new();
                while self.peek()!=Token::Operator(")".into()) {
                    args.push(self.parse_expr()?);
                    if self.peek()==Token::Operator(",".into()) { self.advance(); }
                }
                self.expect_op(")")?;
                e = Expr::Call { func: Box::new(e), args };
            } else if self.peek()==Token::Operator("[".into()) {
                self.advance();
                let idx = self.parse_expr()?;
                self.expect_op("]")?;
                e = Expr::Index { base: Box::new(e), idx: Box::new(idx) };
            } else if self.peek()==Token::Operator("++".into()) {
                self.advance();
                e = Expr::Unary { op: UnaryOp::PostInc, expr: Box::new(e) };
            } else if self.peek()==Token::Operator("--".into()) {
                self.advance();
                e = Expr::Unary { op: UnaryOp::PostDec, expr: Box::new(e) };
            } else {
                break;
            }
        }
        Ok(e)
    }

    fn parse_type(&mut self) -> Result<Type> {
        if let Token::Keyword(ref k) = self.peek() {
            match k.as_str() {
                "char" => { self.advance(); return Ok(Type::Char) }
                "int"  => { self.advance(); return Ok(Type::Int) }
                "void" => { self.advance(); return Ok(Type::Void) }
                _ => {}
            }
        }
        Err(Error::Syntax(format!("Expected type, found {:?}", self.peek())))
    }

    #[inline] fn peek(&self)   -> Token { self.tokens.get(self.pos).cloned().unwrap_or(Token::EOF) }
    #[inline] fn advance(&mut self) { if self.pos < self.tokens.len() { self.pos += 1; }}

    fn expect_op(&mut self, op: &str) -> Result<()> {
        if self.peek()==Token::Operator(op.into()) {
            self.advance(); Ok(())
        } else {
            Err(Error::Syntax(format!("Expected '{}', found {:?}", op, self.peek())))
        }
    }

    fn consume_ident(&mut self) -> Result<String> {
        if let Token::Id(ref s)=self.peek() {
            let i=s.clone(); self.advance(); Ok(i)
        } else {
            Err(Error::Syntax(format!("Expected identifier, found {:?}", self.peek())))
        }
    }

    fn consume_keyword(&mut self, kw: &str) -> Result<()> {
        if let Token::Keyword(ref k)=self.peek() {
            if k==kw { self.advance(); return Ok(()) }
        }
        Err(Error::Syntax(format!("Expected keyword '{}', found {:?}", kw, self.peek())))
    }
}
