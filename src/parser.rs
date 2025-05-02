//! A recursive-descent parser producing an AST for `int f() { return <expr>; }`

use crate::lexer::Token;
use crate::error::{Error, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),

    Function {
        name: String,
        body: Vec<ASTNode>,
    },

    /// A `return <expr>;`
    Return(Box<ASTNode>),

    /// A numeric literal.
    Number(i64),

    /// A binary operation `a + b`, `a * b`, etc.
    BinaryOp {
        op: String,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Create a new parser from a full token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Entry point: parses `Program = Function* EOF`.
    pub fn parse_program(&mut self) -> Result<ASTNode> {
        let mut funcs = Vec::new();
        while self.peek() != Token::EOF {
            funcs.push(self.parse_function()?);
        }
        Ok(ASTNode::Program(funcs))
    }

    fn parse_function(&mut self) -> Result<ASTNode> {
        // expect `int`
        match self.next_token() {
            Token::Keyword(k) if k == "int" => {}
            other => return Err(Error::UnexpectedToken(format!("{:?}", other))),
        }
        // expect name
        let name = match self.next_token() {
            Token::Id(s) => s,
            other        => return Err(Error::UnexpectedToken(format!("{:?}", other))),
        };
        // expect "()"
        self.expect_op("(")?;
        self.expect_op(")")?;
        // parse `{ ... }`
        self.expect_op("{")?;
        let mut body = Vec::new();
        while self.peek() != Token::Operator("}".into()) {
            body.push(self.parse_statement()?);
        }
        self.expect_op("}")?;
        Ok(ASTNode::Function { name, body })
    }

    fn parse_statement(&mut self) -> Result<ASTNode> {
        // only `return` supported
        if let Token::Keyword(ref kw) = self.peek() {
            if kw == "return" {
                self.next_token(); // consume
                let expr = self.parse_expr()?;
                self.expect_op(";")?;
                return Ok(ASTNode::Return(Box::new(expr)));
            }
        }
        Err(Error::InvalidSyntax(format!("Expected `return`, found {:?}", self.peek())))
    }

    fn parse_expr(&mut self) -> Result<ASTNode> {
        // parse a primary, then any number of + or * chains
        let mut node = self.parse_primary()?;
        loop {
            match self.peek() {
                Token::Operator(ref op) if ["+", "-", "*", "/", "%"].contains(&op.as_str()) => {
                    let op = op.clone();
                    self.next_token();
                    let rhs = self.parse_primary()?;
                    node = ASTNode::BinaryOp {
                        op,
                        left: Box::new(node),
                        right: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_primary(&mut self) -> Result<ASTNode> {
        match self.next_token() {
            Token::Num(n) => Ok(ASTNode::Number(n)),
            Token::Operator(ref o) if o == "(" => {
                let inner = self.parse_expr()?;
                self.expect_op(")")?;
                Ok(inner)
            }
            other => Err(Error::UnexpectedToken(format!("{:?}", other))),
        }
    }

    fn expect_op(&mut self, op: &str) -> Result<()> {
        match self.next_token() {
            Token::Operator(s) if s == op => Ok(()),
            other                         => Err(Error::UnexpectedToken(format!("{:?}", other))),
        }
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token::EOF)
    }

    fn next_token(&mut self) -> Token {
        let t = self.peek();
        self.pos += 1;
        t
    }
}
