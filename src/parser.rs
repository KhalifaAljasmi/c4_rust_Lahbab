use crate::compiler::Compiler;
use crate::types::*;
use std::mem;

/// Parses expressions in the source code.
pub fn expr(c: &mut Compiler, lev: Token) -> Result<(), String> {
    let mut t: Type;
    let mut d: usize;

    match c.tk {
        None => return Err(format!("{}: unexpected eof in expression", c.line)),
        Some(Token::Num) => {
            c.e.push(Opcode::IMM as i64);
            c.e.push(c.ival);
            c.lexer_next()?;
            c.ty = Type::INT;
        }
        Some(Token::from(ch)) if ch as u8 == b'"' => {
            c.e.push(Opcode::IMM as i64);
            c.e.push(c.ival);
            c.lexer_next()?;
            while c.tk == Some(Token::from(b'"' as u32)) {
                c.lexer_next()?;
            }
            let align = mem::size_of::<i64>();
            let data_len = (c.data.len() + align - 1) & !(align - 1);
            c.data.resize(data_len, 0);
            c.ty = Type::PTR;
        }
        Some(Token::Sizeof) => {
            c.lexer_next()?;
            if c.tk != Some(Token::from(b'(' as u32)) {
                return Err(format!("{}: open paren expected in sizeof", c.line));
            }
            c.lexer_next()?;
            c.ty = Type::INT;
            if c.tk == Some(Token::Int) {
                c.lexer_next()?;
            } else if c.tk == Some(Token::Char) {
                c.lexer_next()?;
                c.ty = Type::CHAR;
            }
            while c.tk == Some(Token::Mul) {
                c.lexer_next()?;
                c.ty = match c.ty {
                    Type::CHAR => Type::PTR,
                    Type::INT => Type::PTR,
                    Type::PTR => Type::PTR,
                };
            }
            if c.tk != Some(Token::from(b')' as u32)) {
                return Err(format!("{}: close paren expected in sizeof", c.line));
            }
            c.lexer_next()?;
            c.e.push(Opcode::IMM as i64);
            c.e.push(if c.ty == Type::CHAR { 1 } else { 8 });
            c.ty = Type::INT;
        }
        Some(Token::Id) => {
            d = c.id;
            c.lexer_next()?;
            if c.tk == Some(Token::from(b'(' as u32)) {
                c.lexer_next()?;
                let mut nargs = 0;
                while c.tk != Some(Token::from(b')' as u32)) {
                    expr(c, Token::Assign)?;
                    c.e.push(Opcode::PSH as i64);
                    nargs += 1;
                    if c.tk == Some(Token::from(b',' as u32)) {
                        c.lexer_next()?;
                    }
                }
                c.lexer_next()?;
                let class = c.sym[d + IdOffset::Class as usize];
                if class == Token::Sys as i64 {
                    c.e.push(c.sym[d + IdOffset::Val as usize]);
                } else if class == Token::Fun as i64 {
                    c.e.push(Opcode::JSR as i64);
                    c.e.push(c.sym[d + IdOffset::Val as usize]);
                } else {
                    return Err(format!("{}: bad function call", c.line));
                }
                if nargs > 0 {
                    c.e.push(Opcode::ADJ as i64);
                    c.e.push(nargs);
                }
                c.ty = Type::from(c.sym[d + IdOffset::Type as usize] as u32);
            } else if c.sym[d + IdOffset::Class as usize] == Token::Num as i64 {
                c.e.push(Opcode::IMM as i64);
                c.e.push(c.sym[d + IdOffset::Val as usize]);
                c.ty = Type::INT;
            } else {
                if c.sym[d + IdOffset::Class as usize] == Token::Loc as i64 {
                    c.e.push(Opcode::LEA as i64);
                    c.e.push(c.loc - c.sym[d + IdOffset::Val as usize]);
                } else if c.sym[d + IdOffset::Class as usize] == Token::Glo as i64 {
                    c.e.push(Opcode::IMM as i64);
                    c.e.push(c.sym[d + IdOffset::Val as usize]);
                } else {
                    return Err(format!("{}: undefined variable", c.line));
                }
                c.ty = Type::from(c.sym[d + IdOffset::Type as usize] as u32);
                c.e.push(if c.ty == Type::CHAR { Opcode::LC as i64 } else { Opcode::LI as i64 });
            }
        }
        _ => return Err(format!("{}: bad expression", c.line)),
    }

    while c.tk.map_or(false, |t| t >= lev) {
        t = c.ty;
        match c.tk {
            Some(Token::Assign) => {
                c.lexer_next()?;
                if *c.e.last().unwrap_or(&0) == Opcode::LC as i64 || *c.e.last().unwrap_or(&0) == Opcode::LI as i64 {
                    *c.e.last_mut().unwrap() = Opcode::PSH as i64;
                } else {
                    return Err(format!("{}: bad lvalue in assignment", c.line));
                }
                expr(c, Token::Assign)?;
                c.ty = t;
                c.e.push(if c.ty == Type::CHAR { Opcode::SC as i64 } else { Opcode::SI as i64 });
            }
            Some(Token::Mod) => {
                c.lexer_next()?;
                c.e.push(Opcode::PSH as i64);
                expr(c, Token::Inc)?;
                c.e.push(Opcode::MOD as i64);
                c.ty = Type::INT;
            }
            _ => return Err(format!("{}: compiler error tk={:?}", c.line, c.tk)),
        }
    }
    Ok(())
}

/// Parses statements in the source code.
pub fn stmt(c: &mut Compiler) -> Result<(), String> {
    match c.tk {
        Some(Token::If) => {
            c.lexer_next()?;
            if c.tk != Some(Token::from(b'(' as u32)) {
                return Err(format!("{}: open paren expected", c.line));
            }
            c.lexer_next()?;
            expr(c, Token::Assign)?;
            if c.tk != Some(Token::from(b')' as u32)) {
                return Err(format!("{}: close paren expected", c.line));
            }
            c.lexer_next()?;
            c.e.push(Opcode::BZ as i64);
            let b = c.e.len();
            c.e.push(0); // Placeholder
            stmt(c)?;
            if c.tk == Some(Token::Else) {
                c.e[b] = (c.e.len() + 2) as i64;
                c.e.push(Opcode::JMP as i64);
                let b_else = c.e.len();
                c.e.push(0); // Placeholder
                c.lexer_next()?;
                stmt(c)?;
                c.e[b_else] = (c.e.len() + 1) as i64;
            } else {
                c.e[b] = (c.e.len() + 1) as i64;
            }
        }
        Some(Token::While) => {
            c.lexer_next()?;
            let loop_start = c.e.len() + 1;
            if c.tk != Some(Token::from(b'(' as u32)) {
                return Err(format!("{}: open paren expected", c.line));
            }
            c.lexer_next()?;
            expr(c, Token::Assign)?;
            if c.tk != Some(Token::from(b')' as u32)) {
                return Err(format!("{}: close paren expected", c.line));
            }
            c.lexer_next()?;
            c.e.push(Opcode::BZ as i64);
            let b = c.e.len();
            c.e.push(0); // Placeholder
            stmt(c)?;
            c.e.push(Opcode::JMP as i64);
            c.e.push(loop_start as i64);
            c.e[b] = (c.e.len() + 1) as i64;
        }
        Some(Token::Return) => {
            c.lexer_next()?;
            if c.tk != Some(Token::from(b';' as u32)) {
                expr(c, Token::Assign)?;
            }
            c.e.push(Opcode::LEV as i64);
            if c.tk != Some(Token::from(b';' as u32)) {
                return Err(format!("{}: semicolon expected", c.line));
            }
            c.lexer_next()?;
        }
        Some(Token::from(ch)) if ch as u8 == b'{' => {
            c.lexer_next()?;
            while c.tk != Some(Token::from(b'}' as u32)) {
                stmt(c)?;
            }
            c.lexer_next()?;
        }
        Some(Token::from(ch)) if ch as u8 == b';' => {
            c.lexer_next()?;
        }
        _ => {
            expr(c, Token::Assign)?;
            if c.tk != Some(Token::from(b';' as u32)) {
                return Err(format!("{}: semicolon expected", c.line));
            }
            c.lexer_next()?;
        }
    }
    Ok(())
}
