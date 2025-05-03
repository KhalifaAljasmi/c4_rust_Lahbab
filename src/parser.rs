// Modified parser.rs with fixes for control flow parsing
use crate::lexer::Token;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Program(Vec<ASTNode>),
    Function {
        name: String,
        params: Vec<(String, String)>, // Changed to (type, name)
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
        then_branch: Box<ASTNode>,
        else_branch: Option<Box<ASTNode>>,
    },
    Block(Vec<ASTNode>),
    Ternary {
        cond: Box<ASTNode>,
        then_expr: Box<ASTNode>,
        else_expr: Box<ASTNode>,
    },
    While {
        condition: Box<ASTNode>,
        body: Box<ASTNode>,
    },
    For {
        init: Option<Box<ASTNode>>,
        condition: Option<Box<ASTNode>>,
        update: Option<Box<ASTNode>>,
        body: Box<ASTNode>,
    },
    Call {
        name: String,
        args: Vec<ASTNode>,
    },
    CharLiteral(char),
    StringLiteral(String),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),
    UnexpectedEOF(String),
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
                write!(f, "ParseError: Unexpected token {:?}, expected {}", t, expected),
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
        // Parse return type
        self.expect_keyword("int")?;
        // Parse function name
        let name = match self.next_token() {
            Token::Id(s) => s,
            t => return Err(ParseError::UnexpectedToken(t, "function name".to_string())),
        };
        // Check for opening parenthesis
        if self.next_token() != Token::Operator("(".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        // *New Check*: Detect missing closing ')' if next token is '{'
        if self.peek() == Token::Operator("{".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        // Proceed with parsing parameters...
        let mut params = Vec::new();
        if self.peek() != Token::Operator(")".into()) {
            loop {
                let param_type = match self.next_token() {
                    Token::Keyword(s) => s,
                    t => return Err(ParseError::UnexpectedToken(t, "parameter type".to_string())),
                };
                let param_name = match self.next_token() {
                    Token::Id(s) => s,
                    t => return Err(ParseError::UnexpectedToken(t, "parameter name".to_string())),
                };
                params.push((param_type, param_name));
                if self.peek() != Token::Operator(",".into()) {
                    break;
                }
                self.next_token();
            }
        }
        // Check for closing parenthesis
        if self.next_token() != Token::Operator(")".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        let body = self.parse_block()?;
        Ok(ASTNode::Function { 
            name, 
            params, 
            body: match body {
                ASTNode::Block(stmts) => stmts,
                _ => vec![body],
            }
        })
    }
    fn parse_statement(&mut self) -> ParseResult<ASTNode> {
        match self.peek() {
            Token::Keyword(ref k) if k == "return" => {
                self.next_token();  // Consume "return"
                let expr = self.parse_expr()?;
                
                // Check for semicolon after return statement
                match self.peek() {
                    Token::Operator(ref op) if op == ";" => {
                        self.next_token();
                        Ok(ASTNode::Return(Box::new(expr)))
                    },
                    _ => Err(ParseError::MissingSemicolon),
                }
            }
            Token::Keyword(ref k) if k == "if" => {
                self.parse_if_statement()
            },
            Token::Keyword(ref k) if k == "while" => {
                self.parse_while_statement()
            },
            Token::Keyword(ref k) if k == "for" => {
                self.parse_for_statement()
            },
            Token::Operator(ref o) if o == "{" => self.parse_block(),
            _ => {
                let expr = self.parse_expr()?;
                
                // Check for semicolon after expression
                if self.peek() == Token::Operator(";".into()) {
                    self.next_token();
                }
                
                Ok(expr)
            }
        }
    }

    // New dedicated method for parsing if statements
    fn parse_if_statement(&mut self) -> ParseResult<ASTNode> {
        self.next_token();  // Consume "if"
        
        // Check for opening parenthesis
        if self.next_token() != Token::Operator("(".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        let condition = self.parse_expr()?;
        
        // Check for closing parenthesis
        if self.next_token() != Token::Operator(")".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        let then_branch = self.parse_statement()?;
        
        let else_branch = if self.peek() == Token::Keyword("else".into()) {
            self.next_token();  // Consume "else"
            
            // Handle else if
            if self.peek() == Token::Keyword("if".into()) {
                Some(Box::new(self.parse_if_statement()?))
            } else {
                Some(Box::new(self.parse_statement()?))
            }
        } else {
            None
        };
        
        Ok(ASTNode::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    // New dedicated method for parsing while statements
    fn parse_while_statement(&mut self) -> ParseResult<ASTNode> {
        self.next_token();  // Consume "while"
        
        // Check for opening parenthesis
        if self.next_token() != Token::Operator("(".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        let condition = self.parse_expr()?;
        
        // Check for closing parenthesis
        if self.next_token() != Token::Operator(")".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        let body = self.parse_statement()?;
        
        Ok(ASTNode::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    // New dedicated method for parsing for statements
    fn parse_for_statement(&mut self) -> ParseResult<ASTNode> {
        self.next_token();  // Consume "for"
        
        // Check for opening parenthesis
        if self.next_token() != Token::Operator("(".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        // Parse initialization expression
        let init = if self.peek() != Token::Operator(";".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        
        // Check for first semicolon
        if self.next_token() != Token::Operator(";".into()) {
            return Err(ParseError::MissingSemicolon);
        }
        
        // Parse condition expression
        let condition = if self.peek() != Token::Operator(";".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        
        // Check for second semicolon
        if self.next_token() != Token::Operator(";".into()) {
            return Err(ParseError::MissingSemicolon);
        }
        
        // Parse update expression
        let update = if self.peek() != Token::Operator(")".into()) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        
        // Check for closing parenthesis
        if self.next_token() != Token::Operator(")".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        let body = self.parse_statement()?;
        
        Ok(ASTNode::For {
            init,
            condition,
            update,
            body: Box::new(body),
        })
    }

    fn parse_block(&mut self) -> ParseResult<ASTNode> {
        // Check for opening brace
        match self.next_token() {
            Token::Operator(op) if op == "{" => (),
            other => return Err(ParseError::UnexpectedToken(other, "{".to_string())),
        }
        
        let mut stmts = Vec::new();
        
        while self.peek() != Token::Operator("}".into()) && self.peek() != Token::EOF {
            stmts.push(self.parse_statement()?);
        }
        
        // Check for closing brace
        match self.next_token() {
            Token::Operator(op) if op == "}" => (),
            Token::EOF => return Err(ParseError::UnexpectedEOF("}".to_string())),
            other => return Err(ParseError::UnexpectedToken(other, "}".to_string())),
        }
        
        Ok(ASTNode::Block(stmts))
    }

    fn parse_expr(&mut self) -> ParseResult<ASTNode> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseResult<ASTNode> {
        if let Token::Id(name) = self.peek() {
            let name_clone = name.clone();
            self.next_token();
            
            if self.peek() == Token::Operator("=".into()) {
                self.next_token();
                let value = self.parse_ternary()?;
                return Ok(ASTNode::Assignment {
                    name: name_clone,
                    value: Box::new(value),
                });
            } else {
                self.pos -= 1;
            }
        } else if self.peek() == Token::Operator("=".into()) {
            return Err(ParseError::InvalidAssignmentTarget);
        }
        
        self.parse_ternary()
    }

    fn parse_ternary(&mut self) -> ParseResult<ASTNode> {
        let cond = self.parse_logical_or()?;
        
        if self.peek() == Token::Operator("?".into()) {
            self.next_token();
            let then_expr = self.parse_expr()?;
            
            match self.next_token() {
                Token::Operator(op) if op == ":" => (),
                Token::EOF => return Err(ParseError::UnexpectedEOF(":".to_string())),
                other => return Err(ParseError::UnexpectedToken(other, ":".to_string())),
            }
            
            let else_expr = self.parse_expr()?;
            
            return Ok(ASTNode::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            });
        }
        
        Ok(cond)
    }

    fn parse_logical_or(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_logical_and()?;
        
        while self.peek() == Token::Operator("||".into()) {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_logical_and()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_logical_and(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_equality()?;
        
        while self.peek() == Token::Operator("&&".into()) {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_equality()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_equality(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_comparison()?;
        
        while matches!(self.peek(), Token::Operator(ref op) if op == "==" || op == "!=") {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_comparison(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_term()?;
        
        while matches!(self.peek(), Token::Operator(ref op) if op == "<" || op == "<=" || op == ">" || op == ">=") {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_term(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_factor()?;
        
        while matches!(self.peek(), Token::Operator(ref op) if op == "+" || op == "-") {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_factor(&mut self) -> ParseResult<ASTNode> {
        let mut left = self.parse_unary()?;
        
        while matches!(self.peek(), Token::Operator(ref op) if op == "*" || op == "/" || op == "%") {
            let op = match self.next_token() {
                Token::Operator(op_str) => op_str,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            left = ASTNode::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }

    fn parse_unary(&mut self) -> ParseResult<ASTNode> {
        match self.peek() {
            Token::Operator(ref op) if op == "-" || op == "!" || op == "~" => {
                let op = match self.next_token() {
                    Token::Operator(op_str) => op_str,
                    _ => unreachable!(),
                };
                let expr = self.parse_unary()?;
                Ok(ASTNode::UnaryOp {
                    op,
                    expr: Box::new(expr),
                })
            },
            _ => self.parse_call_or_primary(),
        }
    }

    fn parse_call_or_primary(&mut self) -> ParseResult<ASTNode> {
        if let Token::Id(name) = self.peek() {
            let name_clone = name.clone();
            self.next_token();
            
            if self.peek() == Token::Operator("(".into()) {
                self.next_token();
                let args = self.parse_args()?;
                return Ok(ASTNode::Call { name: name_clone, args });
            } else {
                self.pos -= 1;
            }
        }
        
        self.parse_primary()
    }

    fn parse_args(&mut self) -> ParseResult<Vec<ASTNode>> {
        let mut args = Vec::new();
        
        if self.peek() != Token::Operator(")".into()) {
            args.push(self.parse_expr()?);
            
            while self.peek() == Token::Operator(",".into()) {
                self.next_token();
                args.push(self.parse_expr()?);
            }
        }
        
        // Check for closing parenthesis
        if self.next_token() != Token::Operator(")".into()) {
            return Err(ParseError::MissingParenthesis);
        }
        
        Ok(args)
    }

    fn parse_primary(&mut self) -> ParseResult<ASTNode> {
        match self.next_token() {
            Token::Num(n) => Ok(ASTNode::Number(n)),
            Token::Id(name) => Ok(ASTNode::Identifier(name)),
            Token::CharLiteral(c) => Ok(ASTNode::CharLiteral(c)),
            Token::StringLiteral(s) => Ok(ASTNode::StringLiteral(s)),
            Token::Operator(op) if op == "(" => {
                let expr = self.parse_expr()?;
                
                // Check for closing parenthesis
                if self.next_token() != Token::Operator(")".into()) {
                    return Err(ParseError::MissingParenthesis);
                }
                
                Ok(expr)
            },
            Token::EOF => Err(ParseError::UnexpectedEOF("expression".to_string())),
            Token::Unknown(c) => Err(ParseError::UnexpectedToken(
                Token::Unknown(c),
                "valid expression".to_string()
            )),
            token => Err(ParseError::UnexpectedToken(token, "expression".to_string())),
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> ParseResult<()> {
        match self.next_token() {
            Token::Keyword(s) if s == kw => Ok(()),
            Token::EOF => Err(ParseError::UnexpectedEOF(kw.to_string())),
            other => Err(ParseError::UnexpectedToken(other, kw.to_string())),
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

// Modified parse_control_flow function
pub fn parse_control_flow(tokens: Vec<Token>) -> Result<ASTNode, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
