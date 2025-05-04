// src/parser.rs

use crate::lexer::Token;
use std::iter::Peekable;
use std::slice::Iter;

/// Top‐level AST nodes (statements, functions, etc.)
#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Return(Box<Expr>),
    If {
        condition: Box<Expr>,
        then_branch: Box<ASTNode>,
        else_branch: Option<Box<ASTNode>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<ASTNode>,
    },
    Sequence(Vec<ASTNode>),
    Declaration(String, Box<Expr>),
    Assignment(String, Box<Expr>),
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Box<ASTNode>,
    },
    Print(String),
}

/// Expression nodes
#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(i64),
    Variable(String),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
}

/// Parse the entire token stream starting at `main`
pub fn parse(tokens: &[Token]) -> ASTNode {
    let mut iter = tokens.iter().peekable();

    // Find `main` and consume up to its `{`
    loop {
        match iter.next() {
            Some(Token::Identifier(n)) if n == "main" => {
                while let Some(tok) = iter.next() {
                    if *tok == Token::LBrace {
                        break;
                    }
                }
                break;
            }
            Some(_) => continue,
            None => panic!("couldn't find 'main' in tokens"),
        }
    }

    // Collect statements until matching `}`
    let mut stmts = Vec::new();
    while let Some(tok) = iter.peek() {
        match tok {
            Token::Return
            | Token::If
            | Token::While
            | Token::LBrace
            | Token::Int
            | Token::Identifier(_) => {
                stmts.push(parse_stmt(&mut iter))
            }
            Token::RBrace => {
                iter.next();
                break;
            }
            other => panic!("Unexpected token in main: {:?}", other),
        }
    }

    ASTNode::Sequence(stmts)
}

fn parse_stmt(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    // handle printf("..."); first
    if let Some(Token::Identifier(name)) = iter.peek() {
        if name == "printf" {
            iter.next();
            expect_token(iter, Token::LParen);
            let s = match iter.next() {
                Some(Token::StringLiteral(s)) => s.clone(),
                other => panic!("Expected string literal, got {:?}", other),
            };
            expect_token(iter, Token::RParen);
            expect_token(iter, Token::Semicolon);
            return ASTNode::Print(s);
        }
    }

    match iter.peek() {
        Some(Token::Return) => {
            iter.next();
            let e = parse_expr(iter);
            expect_token(iter, Token::Semicolon);
            ASTNode::Return(e)
        }
        Some(Token::If) => {
            iter.next();
            parse_if(iter)
        }
        Some(Token::While) => {
            iter.next();
            parse_while(iter)
        }
        Some(Token::LBrace) => parse_block(iter),
        Some(Token::Int) => {
            iter.next();
            parse_declaration(iter)
        }
        Some(Token::Identifier(_)) => parse_assignment(iter),
        other => panic!("Expected stmt, got {:?}", other),
    }
}

fn parse_block(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    expect_token(iter, Token::LBrace);
    let mut stmts = Vec::new();
    while let Some(tok) = iter.peek() {
        match tok {
            Token::RBrace => {
                iter.next();
                break;
            }
            Token::Return
            | Token::If
            | Token::While
            | Token::LBrace
            | Token::Int
            | Token::Identifier(_) => {
                stmts.push(parse_stmt(iter))
            }
            other => panic!("Bad token in block: {:?}", other),
        }
    }
    ASTNode::Sequence(stmts)
}

fn parse_if(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    expect_token(iter, Token::LParen);
    let cond = parse_expr(iter);
    expect_token(iter, Token::RParen);

    let tb = parse_stmt(iter);
    let eb = if let Some(Token::Else) = iter.peek() {
        iter.next();
        Some(Box::new(parse_stmt(iter)))
    } else {
        None
    };

    ASTNode::If {
        condition: cond,
        then_branch: Box::new(tb),
        else_branch: eb,
    }
}

fn parse_while(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    expect_token(iter, Token::LParen);
    let cond = parse_expr(iter);
    expect_token(iter, Token::RParen);

    let body = parse_stmt(iter);
    ASTNode::While {
        condition: cond,
        body: Box::new(body),
    }
}

fn parse_declaration(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    let name = match iter.next() {
        Some(Token::Identifier(n)) => n.clone(),
        other => panic!("Expected identifier in decl, got {:?}", other),
    };
    expect_token(iter, Token::Assign);
    let expr = parse_expr(iter);
    expect_token(iter, Token::Semicolon);
    ASTNode::Declaration(name, expr)
}

fn parse_assignment(iter: &mut Peekable<Iter<Token>>) -> ASTNode {
    let name = match iter.next() {
        Some(Token::Identifier(n)) => n.clone(),
        other => panic!("Expected identifier in assign, got {:?}", other),
    };
    expect_token(iter, Token::Assign);
    let expr = parse_expr(iter);
    expect_token(iter, Token::Semicolon);
    ASTNode::Assignment(name, expr)
}

fn parse_expr(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    parse_add(iter)
}

fn parse_add(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    let mut node = parse_term(iter);
    while let Some(tok) = iter.peek() {
        match tok {
            Token::Plus => {
                iter.next();
                let rhs = parse_term(iter);
                node = Box::new(Expr::Add(node, rhs));
            }
            Token::Minus => {
                iter.next();
                let rhs = parse_term(iter);
                node = Box::new(Expr::Sub(node, rhs));
            }
            _ => break,
        }
    }
    node
}

fn parse_term(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    let mut node = parse_primary(iter);
    while let Some(tok) = iter.peek() {
        match tok {
            Token::Star => {
                iter.next();
                let rhs = parse_primary(iter);
                node = Box::new(Expr::Mul(node, rhs));
            }
            Token::Divide => {
                iter.next();
                let rhs = parse_primary(iter);
                node = Box::new(Expr::Div(node, rhs));
            }
            Token::Mod => {
                iter.next();
                let rhs = parse_primary(iter);
                node = Box::new(Expr::Mod(node, rhs));
            }
            _ => break,
        }
    }
    node
}

fn parse_primary(iter: &mut Peekable<Iter<Token>>) -> Box<Expr> {
    match iter.next() {
        Some(Token::Number(n)) => Box::new(Expr::Number(*n)),

        Some(Token::Identifier(name)) => {
            let nm = name.clone();
            if let Some(Token::LParen) = iter.peek() {
                iter.next();
                let mut args = Vec::new();
                while let Some(tok2) = iter.peek() {
                    if **tok2 == Token::RParen {
                        break;
                    }
                    let arg = parse_expr(iter);
                    args.push(*arg);
                    if let Some(Token::Comma) = iter.peek() {
                        iter.next();
                    }
                }
                expect_token(iter, Token::RParen);
                Box::new(Expr::Call(nm, args))
            } else {
                Box::new(Expr::Var(nm))
            }
        }

        Some(Token::LParen) => {
            let e = parse_expr(iter);
            expect_token(iter, Token::RParen);
            e
        }

        other => panic!("Expected primary, got {:?}", other),
    }
}

fn expect_token(iter: &mut Peekable<Iter<Token>>, expected: Token) {
    match iter.next() {
        Some(t) if *t == expected => {}
        other => panic!("Expected {:?}, got {:?}", expected, other),
    }
}
