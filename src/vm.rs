// vm.rs
#![allow(dead_code)]  // Add this line to suppress warnings about unused code

use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

use crate::parser::{ASTNode, ParseError};

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    LEA(usize),    // Load local address
    IMM(i32),      // Load immediate value
    JMP(usize),    // Jump
    JSR(usize),    // Jump to subroutine
    BZ(usize),     // Branch if zero
    BNZ(usize),    // Branch if not zero
    ENT(usize),    // Enter subroutine
    ADJ(usize),    // Adjust stack
    LEV,           // Leave subroutine
    LI,            // Load int
    LC,            // Load char
    SI,            // Store int
    SC,            // Store char
    PSH,           // Push
    OR,            // Logical OR
    XOR,           // Logical XOR
    AND,           // Logical AND
    EQ,            // Equal
    NE,            // Not Equal
    LT,            // Less Than
    GT,            // Greater Than
    LE,            // Less Equal
    GE,            // Greater Equal
    SHL,           // Shift Left
    SHR,           // Shift Right
    ADD,           // Add
    SUB,           // Subtract
    MUL,           // Multiply
    DIV,           // Divide
    MOD,           // Modulus
    EXIT,          // Exit program
    PrintfStr(String), // Non-standard: for debugging
}

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
    TypeError(String),
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
            RuntimeError::TypeError(msg) => 
                write!(f, "RuntimeError: Type error - {}", msg),
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

#[allow(dead_code)]
pub struct VM {
    call_stack: Vec<Frame>,
    memory: Memory,
    globals: HashMap<String, Value>,
    builtins: HashSet<String>,
    debug: bool,  // Add debug flag
    cycle: usize, // Add cycle counter for tracing
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
            debug: false,
            cycle: 0,
        }
    }
    
    pub fn with_debug(debug: bool) -> Self {
        let mut vm = Self::new();
        vm.debug = debug;
        vm
    }

    pub fn run(&mut self, ast: ASTNode) -> Result<(), RuntimeError> {
        if self.debug {
            println!("DEBUG: Starting VM execution with AST: {:?}", ast);
        }
        
        // First evaluate all nodes to define functions, globals, etc.
        match &ast {
            ASTNode::Program(nodes) => {
                for node in nodes {
                    self.eval(node)?;
                }
            },
            _ => {
                self.eval(&ast)?;
            }
        }
        
        // Then specifically call the main function if it exists
        if self.globals.contains_key("main") {
            if self.debug {
                println!("DEBUG: Found main function, executing");
            }
            self.call_function("main", &[])?;
        } else {
            return Err(RuntimeError::FunctionNotFound("main".to_string()));
        }
        
        Ok(())
    }

    fn eval(&mut self, node: &ASTNode) -> Result<Value, RuntimeError> {
        // If debug is on, print the current instruction
        if self.debug {
            self.cycle += 1;
            println!("{}> {:?}", self.cycle, node);
        }

        match node {
            ASTNode::Program(nodes) => {
                for node in nodes {
                    self.eval(node)?;
                }
                Ok(Value::Void)
            }
            ASTNode::Function { name, params, body } => {
                // Convert Vec<String> to Vec<(String, String)> by using empty string for type
                let param_tuples: Vec<(String, String)> = params.iter()
                    .map(|p| ("".to_string(), p.clone()))
                    .collect();
                
                self.globals.insert(
                    name.clone(),
                    Value::Function { 
                        params: param_tuples, 
                        body: body.clone()
                    }
                );
                Ok(Value::Void)
            }
            ASTNode::Return(expr) => {
                let value = self.eval(expr)?;
                // REMOVE THIS LINE: self.call_stack.pop();
                // The call stack is already popped in the call_function method,
                // popping it again here causes us to lose the main function's frame
                Ok(value)
            }
            ASTNode::Number(n) => Ok(Value::Int((*n).into())), // Convert i32 to i64
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
                
                // First check local variables in call stack frames
                for frame in self.call_stack.iter_mut().rev() {
                    if let Some(existing) = frame.locals.get_mut(name) {
                        *existing = val.clone();
                        updated = true;
                        break;
                    }
                }
                
                // If not found in local scopes, check in globals
                if !updated {
                    if self.globals.contains_key(name) {
                        self.globals.insert(name.clone(), val.clone());
                        updated = true;
                    }
                }
                
                // If still not found anywhere, create it in the current scope
                if !updated {
                    if let Some(frame) = self.call_stack.last_mut() {
                        frame.locals.insert(name.clone(), val.clone());
                    } else {
                        self.globals.insert(name.clone(), val.clone());
                    }
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
                
                while condition.as_ref().map_or(true, |cond| {
                    self.eval_condition(cond).unwrap_or(false)
                }) {
                    self.eval(body)?;
                    
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
            ASTNode::Expression(_expr) => {
                // Handle Expr compatibility
                // ... implementation ...
                Ok(Value::Int(0)) // Placeholder
            }
            ASTNode::VariableDeclaration { var_type: _, name, initializer } => {
                let value = if let Some(init) = initializer {
                    self.eval(init)?
                } else {
                    // Default initialize based on type (0 for int/char)
                    Value::Int(0)
                };
                
                // Add the variable to the current frame's locals
                if let Some(frame) = self.call_stack.last_mut() {
                    frame.locals.insert(name.clone(), value.clone());
                } else {
                    // If not in a function, add to globals
                    self.globals.insert(name.clone(), value.clone());
                }
                
                Ok(Value::Void)
            },
        }
    }

    fn eval_condition(&mut self, condition: &Box<ASTNode>) -> Result<bool, RuntimeError> {
        let value = self.eval(condition)?;
        match value {
            Value::Int(n) => Ok(n != 0),
            Value::Bool(b) => Ok(b),
            _ => Err(RuntimeError::TypeError("Condition must be a boolean or integer".into()))
        }
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

    #[allow(dead_code)]
    fn eval_value(&self, value: Value) -> Result<i64, RuntimeError> {
        match value {
            Value::Int(n) => Ok(n),
            Value::Bool(b) => Ok(if b { 1 } else { 0 }),
            Value::String(_) => Err(RuntimeError::TypeError("Cannot convert string to integer".into())),
            Value::Pointer(p) => Ok(p as i64),
            Value::Void => Ok(0),
            Value::Function { .. } => Err(RuntimeError::TypeError("Cannot convert function to integer".into())),
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
        if self.debug {
            println!("DEBUG: Calling function {} with {} args", name, args.len());
        }
        
        // Special case for printf
        if name == "printf" {
            return self.handle_printf(args);
        }
    
        // Get function from globals
        let func = match self.globals.get(name) {
            Some(Value::Function { params, body }) => (params.clone(), body.clone()),
            _ => return Err(RuntimeError::FunctionNotFound(name.to_string())),
        };
    
        // Check argument count
        if args.len() != func.0.len() {
            return Err(RuntimeError::InvalidFunctionCall(format!(
                "Expected {} arguments for function '{}', got {}",
                func.0.len(), name, args.len()
            )));
        }
    
        // Evaluate arguments
        let evaluated_args = args.iter()
            .map(|arg| self.eval(arg))
            .collect::<Result<Vec<_>, _>>()?;
    
        // Create new stack frame
        let mut frame = Frame {
            locals: HashMap::new(),
        };
    
        // Bind arguments to parameters
        for ((_, param_name), value) in func.0.iter().zip(evaluated_args) {
            frame.locals.insert(param_name.clone(), value);
        }
    
        // Push frame and execute function body
        self.call_stack.push(frame);
        
        // Execute function body statements
        let mut result = Value::Void;
        for stmt in &func.1 {
            result = self.eval(stmt)?;
            // If we got a non-void result (from a return statement),
            // break out of the loop
            if !matches!(result, Value::Void) {
                break;
            }
        }
    
        // Debug output to verify result
        if self.debug {
            println!("DEBUG: Function {} returned {:?}", name, result);
        }
    
        // Always pop the frame after function execution
        self.call_stack.pop();
        Ok(result)
    }

    fn handle_printf(&mut self, args: &[ASTNode]) -> Result<Value, RuntimeError> {
        match args.get(0) {
            Some(ASTNode::StringLiteral(fmt_str)) => {
                let values = args[1..].iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                
                self.format_and_print(fmt_str, &values);
                Ok(Value::Int(0))
            },
            // Also handle the case where a variable contains the format string
            Some(ASTNode::Identifier(name)) => {
                let format_value = self.eval(&ASTNode::Identifier(name.clone()))?;
                
                let fmt_str = match format_value {
                    Value::String(s) => s,
                    Value::Pointer(addr) => {
                        self.memory.read_string(addr).unwrap_or_else(|| "?invalid_format?".to_string())
                    },
                    _ => return Err(RuntimeError::TypeMismatch(
                        "First argument to printf must be a string".to_string()
                    )),
                };
                
                let values = args[1..].iter()
                    .map(|arg| self.eval(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                
                self.format_and_print(&fmt_str, &values);
                Ok(Value::Int(0))
            },
            _ => Err(RuntimeError::TypeMismatch(
                "First argument to printf must be a string".to_string()
            )),
        }
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
                            if let Some(c) = chars.next() {
                                result.push(c);
                            }
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
        
        // Use print! instead of println! to avoid adding extra newlines
        print!("{}", result);
        io::stdout().flush().unwrap(); // Make sure to flush the output
    }
    
    // Add helper to print formatted instructions like in c4
    fn print_instruction(&self, instr_name: &str, value: Option<i64>) {
        match value {
            Some(val) => println!("{}> {:<4} {}", self.cycle, instr_name, val),
            None => println!("{}> {:<4}", self.cycle, instr_name),
        }
    }
}