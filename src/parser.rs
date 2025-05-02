//! A recursive-descent parser producing an AST for a C-like language

use crate::lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<ASTNode>,
    },
    Return(Box<ASTNode>),
    Number(i64),
    Identifier(String),
    Assignment {
        name: String,
        value: Box<ASTNode>,
    },
    UnaryOp {
        op: String,
        expr: Box<ASTNode>,
    },
    BinaryOp {
        op: String,
        left: Box<ASTNode>,
        right: Box<ASTNode>,
    },
    If {
        condition: Box<ASTNode>,
        then_branch: Vec<ASTNode>,
        else_branch: Option<Vec<ASTNode>>,
    },
    Block(Vec<ASTNode>),
    Ternary {
        cond: Box<ASTNode>,
        then_expr: Box<ASTNode>,
        else_expr: Box<ASTNode>,
    },
    While {
        condition: Box<ASTNode>,
        body: Vec<ASTNode>,
    },
    For {
        init: Option<Box<ASTNode>>,
        condition: Option<Box<ASTNode>>,
        update: Option<Box<ASTNode>>,
        body: Vec<ASTNode>,
    },
    Call {
        name: String,
        args: Vec<ASTNode>,
    },
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),  // Changed to owned String
    UnexpectedEOF(String),          // Changed to owned String
    InvalidAssignmentTarget,
    InvalidParameter,
    InvalidExpression,
    MissingSemicolon,
    MissingBrace,
    MissingParenthesis,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(t, expected) =>
                write!(f, "ParseError: Unexpected token, expected {}, found {:?}", expected, t),
            ParseError::UnexpectedEOF(expected) =>
                write!(f, "ParseError: Unexpected EOF, expected {}", expected),
            ParseError::InvalidAssignmentTarget =>
                write!(f, "ParseError: Invalid assignment target"),
            ParseError::InvalidParameter =>
                write!(f, "ParseError: Invalid parameter"),
            ParseError::InvalidExpression =>
                write!(f, "ParseError: Invalid expression"),
            ParseError::MissingSemicolon =>
                write!(f, "ParseError: Missing semicolon"),
            ParseError::MissingBrace =>
                write!(f, "ParseError: Missing brace"),
            ParseError::MissingParenthesis =>
                write!(f, "ParseError: Missing parenthesis"),
        }
    }
}

impl std::error::Error for ParseError {}

type ParseResult<T> = std::result::Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self) -> ParseResult<ASTNode> {
        let mut nodes = Vec::new();
        while self.peek() != Token::EOF {
            nodes.push(self.parse_function()?);
        }
        Ok(ASTNode::Program(nodes))
    }

    fn parse_function(&mut self) -> ParseResult<ASTNode> {
        // int
        self.expect_keyword("int")?;

        // function name
        let name = match self.next_token() {
            Token::Id(s) => s,
            other => return Err(ParseError::UnexpectedToken(other, "function name".into())),
        };

        // (
        self.expect_op("(")?;
        let mut params = Vec::new();

        // only parse params if we're not immediately at ')'
        if self.peek() != Token::Operator(")".into()) {
            loop {
                // missing the closing ')'?
                if self.peek() == Token::Operator("{".into()) {
                    return Err(ParseError::MissingParenthesis);
                }

                // must be an identifier
                match self.next_token() {
                    Token::Id(p) => params.push(p),
                    _ => return Err(ParseError::MissingParenthesis),
                }

                // comma → another param, otherwise end of list
                if self.peek() == Token::Operator(",".into()) {
                    self.next_token();
                    continue;
                }
                break;
            }
        }

        // )
        self.expect_op(")")?;

        // {
        self.expect_op("{")?;
        let mut body = Vec::new();
        while self.peek() != Token::Operator("}".into()) {
            body.push(self.parse_statement()?);
        }
        // }
        self.expect_op("}")?;

        Ok(ASTNode::Function { name, params, body })
    }
}

    fn parse_statement(&mut self) -> ParseResult<ASTNode> {
        match self.peek() {
            Token::Keyword(ref k) if k == "return" => {
                self.next_token();
                let expr = self.parse_expr()?;
                if self.peek() == Token::Operator(";".into()) {
                    self.next_token();
                } else {
                    return Err(ParseError::MissingSemicolon);
                }
                Ok(ASTNode::Return(Box::new(expr)))
            }
            Token::Keyword(ref k) if k == "if" => self.parse_if(),
            Token::Keyword(ref k) if k == "while" => self.parse_while(),
            Token::Keyword(ref k) if k == "for" => self.parse_for(),
            Token::Operator(ref o) if o == "{" => self.parse_block(),
            _ if matches!(
                self.peek(),
                Token::Id(_) 
                | Token::Num(_)
                | Token::Operator(ref o) if ["-", "!", "~", "("].contains(&o.as_str())
            ) => {
                let expr = self.parse_expr()?;
                if self.peek() == Token::Operator(";".into()) {
                    self.next_token();
                }
                Ok(expr)
            }
            _ => Err(ParseError::InvalidExpression),
        }
    }

    fn parse_if(&mut self) -> ParseResult<ASTNode> {
        self.expect_keyword("if")?;
        self.expect_op("(")?;
        let condition = self.parse_expr()?;
        self.expect_op(")")?;

        let then_branch = self.parse_block_items()?;
        let else_branch = if self.peek() == Token::Keyword("else".into()) {
            self.next_token();
            if self.peek() == Token::Keyword("if".into()) {
                // else if → nested If
                let nested = self.parse_if()?;
                Some(vec![nested])
            } else {
                // plain else { ... }
                Some(self.parse_block_items()?)
            }
        } else {
            None
        };

        Ok(ASTNode::If {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        })
    }

    fn parse_while(&mut self) -> ParseResult<ASTNode> {
        self.expect_keyword("while")?;
        self.expect_op("(")?;
        let condition = self.parse_expr()?;
        self.expect_op(")")?;
        
        let body = self.parse_block_items()?;
        Ok(ASTNode::While {
            condition: Box::new(condition),
            body,
        })
    }

    fn parse_for(&mut self) -> ParseResult<ASTNode> {
        self.expect_keyword("for")?;
        self.expect_op("(")?;
        
        let init = if self.peek() != Token::Operator(";".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect_op(";")?;
        
        let condition = if self.peek() != Token::Operator(";".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect_op(";")?;
        
        let update = if self.peek() != Token::Operator(")".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect_op(")")?;
        
        let body = self.parse_block_items()?;
        
        Ok(ASTNode::For {
            init,
            condition,
            update,
            body,
        })
    }

    fn parse_block_items(&mut self) -> ParseResult<Vec<ASTNode>> {
        if self.peek() != Token::Operator("{".into()) {
            return Err(ParseError::MissingBrace);
        }
        self.next_token();
        
        let mut stmts = Vec::new();
        while self.peek() != Token::Operator("}".into()) {
            stmts.push(self.parse_statement()?);
        }
        self.expect_op("}")?;
        
        Ok(stmts)
    }

    fn parse_block(&mut self) -> ParseResult<ASTNode> {
        Ok(ASTNode::Block(self.parse_block_items()?))
    }

    fn parse_expr(&mut self) -> ParseResult<ASTNode> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> ParseResult<ASTNode> {
        let mut left = self.parse_unary()?;

        while let Token::Operator(op) = self.peek() {
            // only handle real binary ops; allow '?' but never treat ':' as a generic op
            if op == ":" {
                break;
            }
            let prec = self.precedence(&op);
            if prec < min_prec || prec == 0 {
                break;
            }

            if op == "?" {
                self.next_token();
                let then_expr = self.parse_expr()?;
                self.expect_op(":")?;
                let else_expr = self.parse_expr()?;
                left = ASTNode::Ternary {
                    cond: Box::new(left),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                };
                continue;
            }

            self.next_token();
            let right = self.parse_binary_expr(prec + 1)?;
            if op == "=" {
                if let ASTNode::Identifier(name) = left {
                    left = ASTNode::Assignment {
                        name,
                        value: Box::new(right),
                    };
                } else {
                    return Err(ParseError::InvalidAssignmentTarget);
                }
            } else {
                left = ASTNode::BinaryOp {
                    op: op.clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> ParseResult<ASTNode> {
        if let Token::Operator(op) = self.peek() {
            if ["-", "!", "~"].contains(&op.as_str()) {
                self.next_token();
                let expr = self.parse_unary()?;
                return Ok(ASTNode::UnaryOp {
                    op: op.clone(),
                    expr: Box::new(expr),
                });
            }
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<ASTNode> {
        match self.next_token() {
            Token::Num(n) => Ok(ASTNode::Number(n)),
            Token::Id(s) => {
                if self.peek() == Token::Operator("(".into()) {
                    self.parse_call(s)
                } else {
                    Ok(ASTNode::Identifier(s))
                }
            }
            Token::Operator(op) if op == "(" => {
                let expr = self.parse_expr()?;
                self.expect_op(")")?;
                Ok(expr)
            }
            t => Err(ParseError::UnexpectedToken(t, "expression".to_string())),
        }
    }

    fn parse_call(&mut self, name: String) -> ParseResult<ASTNode> {
        self.expect_op("(")?;
        let mut args = Vec::new();
        
        while self.peek() != Token::Operator(")".into()) {
            args.push(self.parse_expr()?);
            if self.peek() == Token::Operator(",".into()) {
                self.next_token();
            }
        }
        self.expect_op(")")?;
        
        Ok(ASTNode::Call { name, args })
    }

    fn precedence(&self, op: &str) -> u8 {
        match op {
            "=" => 1,
            "?" => 2,
            ":" => 3,
            "||" => 4,
            "&&" => 5,
            "==" | "!=" => 6,
            "<" | "<=" | ">" | ">=" => 7,
            "+" | "-" => 8,
            "*" | "/" | "%" => 9,
            _ => 0,
        }
    }

    fn expect_op(&mut self, op: &str) -> ParseResult<()> {
        match self.next_token() {
            Token::Operator(s) if s == op => Ok(()),
            other => Err(ParseError::UnexpectedToken(other, op.to_string())),
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> ParseResult<()> {
        match self.next_token() {
            Token::Keyword(s) if s == kw => Ok(()),
            other => Err(ParseError::UnexpectedToken(other, kw.to_string())),
        }
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.pos).cloned().unwrap_or(Token::EOF)
    }

    fn peek_ahead(&self, n: usize) -> Token {
        self.tokens.get(self.pos + n).cloned().unwrap_or(Token::EOF)
    }

    fn next_token(&mut self) -> Token {
        let t = self.peek();
        self.pos += 1;
        t
    }
}


