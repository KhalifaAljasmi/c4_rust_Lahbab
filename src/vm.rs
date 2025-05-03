// vm.rs

use std::collections::{HashMap, HashSet};

use crate::parser::{ASTNode, ParseError};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Pointer(usize),
    Void,
    Function { 
        params: Vec<(String, String)>, 
        body: Vec<ASTNode>,
    },
}

#[derive(Debug)]
pub enum RuntimeError {
    DivisionByZero,
    VariableNotFound(String),
    FunctionNotFound(String),
    TypeMismatch(String),
    InvalidOperation,
    UnexpectedEOF,
    InvalidAssignment,
    InvalidFunctionCall(String),
    ParseError(ParseError),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::DivisionByZero => 
                write!(f, "RuntimeError: Division by zero"),
            RuntimeError::VariableNotFound(var) => 
                write!(f, "RuntimeError: Variable '{}' not found", var),
            RuntimeError::FunctionNotFound(func) => 
                write!(f, "RuntimeError: Function '{}' not found", func),
            RuntimeError::TypeMismatch(msg) => 
                write!(f, "RuntimeError: Type mismatch - {}", msg),
            RuntimeError::InvalidOperation => 
                write!(f, "RuntimeError: Invalid operation"),
            RuntimeError::UnexpectedEOF => 
                write!(f, "RuntimeError: Unexpected end of input"),
            RuntimeError::InvalidAssignment => 
                write!(f, "RuntimeError: Invalid assignment"),
            RuntimeError::InvalidFunctionCall(name) => 
                write!(f, "RuntimeError: Invalid function call to '{}'", name),
            RuntimeError::ParseError(e) => 
                write!(f, "Parse error: {}", e),
        }
    }
}

impl From<ParseError> for RuntimeError {
    fn from(e: ParseError) -> Self {
        RuntimeError::ParseError(e)
    }
}

struct Memory {
    heap: HashMap<usize, Vec<u8>>,
    string_table: HashMap<String, usize>,
    next_addr: usize,
}

impl Memory {
    fn new() -> Self {
        Self {
            heap: HashMap::new(),
            string_table: HashMap::new(),
            next_addr: 0x1000,
        }
    }

    fn alloc_string(&mut self, s: &str) -> usize {
        if let Some(&addr) = self.string_table.get(s) {
            return addr;
        }
        let addr = self.next_addr;
        self.string_table.insert(s.to_string(), addr);
        
        let bytes: Vec<u8> = s.bytes().chain(std::iter::once(0)).collect();
        self.heap.insert(addr, bytes);
        
        self.next_addr += s.len() + 1;
        addr
    }

    fn read_string(&self, addr: usize) -> Option<String> {
        let bytes = self.heap.get(&addr)?;
        Some(
            bytes.iter()
                .take_while(|&&b| b != 0)
                .map(|&b| b as char)
                .collect::<String>()
        )
    }
}

struct Frame {
    locals: HashMap<String, Value>,
}

pub struct VM {
    call_stack: Vec<Frame>,
    memory: Memory,
    globals: HashMap<String, Value>,
    builtins: HashSet<String>,
}

impl VM {
    pub fn new() -> Self {
        let mut builtins = HashSet::new();
        builtins.insert("printf".to_string());
        
        Self {
            call_stack: vec![Frame {
                locals: HashMap::new(),
            }],
            memory: Memory::new(),
            globals: HashMap::new(),
            builtins,
        }
    }

    pub fn run(&mut self, ast: ASTNode) -> Result<(), RuntimeError> {
        match ast {
            ASTNode::Program(nodes) => {
                for node in nodes {
                    self.eval(&node)?;
                }
                Ok(())
            },
            _ => {
                self.eval(&ast)?;
                Ok(())
            }
        }
    }

    fn eval(&mut self, node: &ASTNode) -> Result<Value, RuntimeError> {
        match node {
            ASTNode::Program(nodes) => {
                for node in nodes {
                    self.eval(node)?;
                }
                Ok(Value::Void)
            }
            ASTNode::Function { name, params, body } => {
                self.globals.insert(
                    name.clone(),
                    Value::Function { 
                        params: params.clone(), 
                        body: body.clone()
                    }
                );
                Ok(Value::Void)
            }
            ASTNode::Return(expr) => {
                let value = self.eval(expr)?;
                self.call_stack.pop();
                Ok(value)
            }
            ASTNode::Number(n) => Ok(Value::Int(*n)),
            ASTNode::Identifier(name) => {
                for frame in self.call_stack.iter().rev() {
                    if let Some(value) = frame.locals.get(name) {
                        return Ok(value.clone());
                    }
                }
                if let Some(value) = self.globals.get(name) {
                    return Ok(value.clone());
                }
                Err(RuntimeError::VariableNotFound(name.clone()))
            }
            ASTNode::Assignment { name, value } => {
                let val = self.eval(value)?;
                
                let mut updated = false;
                for frame in self.call_stack.iter_mut().rev() {
                    if let Some(existing) = frame.locals.get_mut(name) {
                        *existing = val.clone();
                        updated = true;
                        break;
                    }
                }
                
                if !updated {
                    self.call_stack.last_mut().unwrap().locals.insert(name.clone(), val.clone());
                }
                
                Ok(val)
            }
            ASTNode::UnaryOp { op, expr } => {
                let val = self.eval(expr)?;
                self.eval_unary_op(op, val)
            }
            ASTNode::BinaryOp { op, left, right } => {
                let l = self.eval(left)?;
                let r = self.eval(right)?;
                self.eval_binary_op(op, l, r)
            }
            ASTNode::If { condition, then_branch, else_branch } => {
                let cond = self.eval(condition)?;
                if self.is_truthy(&cond) {
                    self.eval(then_branch)
                } else if let Some(else_node) = else_branch {
                    self.eval(else_node)
                } else {
                    Ok(Value::Void)
                }
            }
            ASTNode::While { condition, body } => {
                while self.eval_condition(condition)? {
                    self.eval(body)?;
                }
                Ok(Value::Void)
            }
            ASTNode::For { init, condition, update, body } => {
                if let Some(init_expr) = init {
                    self.eval(init_expr)?;
                }
                
                while self.eval_condition(condition.as_ref().unwrap_or(&Box::new(ASTNode::Number(1))))? {
                    let result = self.eval(body)?;
                    
                    if let Value::Void = result {
                        // Continue looping
                    } else {
                        return Ok(result);
                    }
                    
                    if let Some(update_expr) = update {
                        self.eval(update_expr)?;
                    }
                }
                
                Ok(Value::Void)
            }
            ASTNode::Call { name, args } => {
                self.call_function(name, args)
            }
            ASTNode::Ternary { cond, then_expr, else_expr } => {
                let condition = self.eval(cond)?;
                if self.is_truthy(&condition) {
                    self.eval(then_expr)
                } else {
                    self.eval(else_expr)
                }
            }
            ASTNode::CharLiteral(c) => Ok(Value::Int(*c as i64)),
            ASTNode::StringLiteral(s) => {
                let addr = self.memory.alloc_string(s);
                Ok(Value::Pointer(addr))
            }
            ASTNode::Block(stmts) => {
                for stmt in stmts {
                    self.eval(stmt)?;
                }
                Ok(Value::Void)
            }
            _ => Ok(Value::Void),
        }
    }

    fn eval_condition(&mut self, condition: &Box<ASTNode>) -> Result<bool, RuntimeError> {
        self.eval(condition).map(|val| self.is_truthy(&val))
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Int(n) => *n != 0,
            Value::Bool(b) => *b,
            Value::Pointer(addr) => *addr != 0,
            Value::String(s) => !s.is_empty(),
            Value::Void => false,
            Value::Function { .. } => true,
        }
    }

    fn eval_unary_op(&self, op: &str, value: Value) -> Result<Value, RuntimeError> {
        match op {
            "-" => match value {
                Value::Int(n) => Ok(Value::Int(-n)),
                _ => Err(RuntimeError::TypeMismatch("Expected integer for negation".to_string())),
            },
            "!" => match value {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Ok(Value::Bool(!self.is_truthy(&value))),
            },
            _ => Err(RuntimeError::InvalidOperation),
        }
    }

    fn eval_binary_op(&self, op: &str, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match op {
            "+" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Pointer(a), Value::Int(b)) => Ok(Value::Pointer(a + b as usize)),
                (Value::Int(a), Value::Pointer(b)) => Ok(Value::Pointer(b + a as usize)),
                _ => Err(RuntimeError::TypeMismatch("Expected integers for addition".to_string())),
            },
            "-" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Pointer(a), Value::Int(b)) => Ok(Value::Pointer(a - b as usize)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Int((a - b) as i64)),
                _ => Err(RuntimeError::TypeMismatch("Expected integers for subtraction".to_string())),
            },
            "*" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                _ => Err(RuntimeError::TypeMismatch("Expected integers for multiplication".to_string())),
            },
            "/" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => {
                    if b == 0 {
                        Err(RuntimeError::DivisionByZero)
                    } else {
                        Ok(Value::Int(a / b))
                    }
                },
                _ => Err(RuntimeError::TypeMismatch("Expected integers for division".to_string())),
            },
            "==" => Ok(Value::Bool(left == right)),
            "!=" => Ok(Value::Bool(left != right)),
            "<" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Bool(a < b)),
                _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
            },
            ">" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Bool(a > b)),
                _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
            },
            "<=" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Bool(a <= b)),
                _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
            },
            ">=" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                (Value::Pointer(a), Value::Pointer(b)) => Ok(Value::Bool(a >= b)),
                _ => Err(RuntimeError::TypeMismatch("Invalid types for comparison".to_string())),
            },
            _ => Err(RuntimeError::InvalidOperation),
        }
    }

    fn call_function(&mut self, name: &str, args: &[ASTNode]) -> Result<Value, RuntimeError> {
        if name == "printf" {
            return self.handle_printf(args);
        }

        let func = match self.globals.get(name) {
            Some(Value::Function { params, body }) => (params.clone(), body.clone()),
            _ => return Err(RuntimeError::FunctionNotFound(name.to_string())),
        };

        if args.len() != func.0.len() {
            return Err(RuntimeError::InvalidFunctionCall(format!(
                "Expected {} arguments for function '{}', got {}",
                func.0.len(), name, args.len()
            )));
        }

        let evaluated_args = args.iter()
            .map(|arg| self.eval(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let mut frame = Frame {
            locals: HashMap::new(),
        };

        for ((_, param_name), value) in func.0.iter().zip(evaluated_args) {
            frame.locals.insert(param_name.clone(), value);
        }

        self.call_stack.push(frame);
        let mut result = Value::Void;

        for stmt in &func.1 {
            result = self.eval(stmt)?;
            if let Value::Void = result {
                continue;
            }
        }

        self.call_stack.pop();
        Ok(result)
    }

    fn handle_printf(&mut self, args: &[ASTNode]) -> Result<Value, RuntimeError> {
        if let Some(ASTNode::StringLiteral(fmt_str)) = args.get(0) {
            let values = args[1..].iter()
                .map(|arg| self.eval(arg))
                .collect::<Result<Vec<_>, _>>()?;
            
            self.format_and_print(fmt_str, &values);
            return Ok(Value::Int(0));
        }
        
        Err(RuntimeError::TypeMismatch("First argument to printf must be a string".to_string()))
    }

    fn format_and_print(&self, fmt_str: &str, args: &[Value]) {
        let mut result = String::new();
        let mut chars = fmt_str.chars().peekable();
        
        let mut arg_idx = 0;
        
        while let Some(c) = chars.next() {
            if c == '%' && chars.peek() == Some(&'%') {
                chars.next();
                result.push('%');
            } else if c == '%' {
                if arg_idx < args.len() {
                    let arg = &args[arg_idx];
                    match chars.next() {
                        Some('d') | Some('i') => {
                            if let Value::Int(n) = arg {
                                result.push_str(&n.to_string());
                            } else {
                                result.push_str("?int?");
                            }
                        },
                        Some('c') => {
                            match arg {
                                Value::Int(n) => result.push(*n as u8 as char),
                                Value::Pointer(addr) => {
                                    if let Some(c) = self.memory.read_string(*addr) {
                                        result.push_str(&c);
                                    }
                                },
                                _ => {}
                            }
                        },
                        Some('s') => {
                            match arg {
                                Value::Pointer(addr) => {
                                    if let Some(s) = self.memory.read_string(*addr) {
                                        result.push_str(&s);
                                    }
                                },
                                Value::String(s) => result.push_str(s),
                                _ => result.push_str("?str?"),
                            }
                        },
                        Some('p') => {
                            match arg {
                                Value::Pointer(addr) => {
                                    let ptr = *addr as *const ();
                                    result.push_str(&format!("{:p}", ptr));
                                },
                                Value::Int(n) => {
                                    let ptr = *n as usize as *const ();
                                    result.push_str(&format!("{:p}", ptr));
                                },
                                _ => result.push_str("?ptr?"),
                            }
                        },
                        Some('x') => {
                            if let Value::Int(n) = arg {
                                result.push_str(&format!("{:x}", n));
                            } else {
                                result.push_str("?hex?");
                            }
                        },
                        Some('X') => {
                            if let Value::Int(n) = arg {
                                result.push_str(&format!("{:X}", n));
                            } else {
                                result.push_str("?HEX?");
                            }
                        },
                        _ => {
                            result.push('%');
                            result.push('?');
                        }
                    }
                    arg_idx += 1;
                } else {
                    result.push_str("%?");
                }
            } else {
                result.push(c);
            }
        }
        
        print!("{}", result);
    }
}