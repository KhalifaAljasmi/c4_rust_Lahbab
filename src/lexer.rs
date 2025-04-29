// lexer.rs

// Define the different types of tokens we can find
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(i64),           // Number literals (e.g., 123, 0xFF)
    Id(String),         // Identifiers (e.g., variable names)
    Keyword(String),    // Keywords (e.g., int, return)
    Operator(String),   // Operators (e.g., +, ==, &&)
    CharLiteral(char),  // Character literals (e.g., 'a')
    StringLiteral(String), // String literals (e.g., "hello")
    EOF,                // End of file/input
    Unknown(char),      // Anything we don't recognize
}

// Lexer struct to manage the input and current position
pub struct Lexer {
    input: Vec<char>,   // Source code as list of characters
    pub pos: usize,     // Current position/index in input
}

impl Lexer {
    // Create a new lexer from a source string
    pub fn new(source: &str) -> Self {
        Self {
            input: source.chars().collect(),
            pos: 0,
        }
    }

    // Get the next character and advance position
    pub fn next_char(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            None
        } else {
            let ch = self.input[self.pos];
            self.pos += 1;
            Some(ch)
        }
    }

    // Look ahead at the next character without moving
    pub fn peek_char(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    // Main function to return the next token
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        match self.next_char() {
            Some(ch) => {
                match ch {
                    // Handle identifiers and keywords
                    c if c.is_ascii_alphabetic() || c == '_' => {
                        let mut ident = c.to_string();
                        while let Some(nc) = self.peek_char() {
                            if nc.is_ascii_alphanumeric() || nc == '_' {
                                ident.push(self.next_char().unwrap());
                            } else {
                                break;
                            }
                        }
                        match ident.as_str() {
                            // Check if it's a known keyword
                            "char" | "else" | "enum" | "if" | "int" | "return" |
                            "sizeof" | "while" | "open" | "read" | "close" | 
                            "printf" | "malloc" | "free" | "memset" | "memcmp" | 
                            "exit" | "void" | "main" => Token::Keyword(ident),
                            _ => Token::Id(ident),
                        }
                    }

                    // Handle numeric literals (decimal, hex, octal)
                    c if c.is_ascii_digit() => self.lex_number(c),

                    // Handle character literals like 'a'
                    '\'' => self.lex_char_literal(),

                    // Handle string literals like "hello"
                    '"' => self.lex_string_literal(),

                    // Handle multi-character and single-character operators
                    '=' => {
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            Token::Operator("==".to_string())
                        } else {
                            Token::Operator("=".to_string())
                        }
                    }
                    '!' => {
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            Token::Operator("!=".to_string())
                        } else {
                            Token::Operator("!".to_string())
                        }
                    }
                    '<' => {
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            Token::Operator("<=".to_string())
                        } else if self.peek_char() == Some('<') {
                            self.next_char();
                            Token::Operator("<<".to_string())
                        } else {
                            Token::Operator("<".to_string())
                        }
                    }
                    '>' => {
                        if self.peek_char() == Some('=') {
                            self.next_char();
                            Token::Operator(">=".to_string())
                        } else if self.peek_char() == Some('>') {
                            self.next_char();
                            Token::Operator(">>".to_string())
                        } else {
                            Token::Operator(">".to_string())
                        }
                    }
                    '&' => {
                        if self.peek_char() == Some('&') {
                            self.next_char();
                            Token::Operator("&&".to_string())
                        } else {
                            Token::Operator("&".to_string())
                        }
                    }
                    '|' => {
                        if self.peek_char() == Some('|') {
                            self.next_char();
                            Token::Operator("||".to_string())
                        } else {
                            Token::Operator("|".to_string())
                        }
                    }

                    // Handle simple single-character operators
                    '+' | '-' | '*' | '/' | '%' | '^' | '~' | '?' |
                    '[' | ']' | '{' | '}' | '(' | ')' | ';' | ':' | ',' => {
                        Token::Operator(ch.to_string())
                    }

                    // Anything unknown
                    _ => Token::Unknown(ch),
                }
            }
            None => Token::EOF,
        }
    }

    // Skip whitespaces and line comments (// and #)
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    self.next_char();
                }
                Some('/') => {
                    if self.input.get(self.pos + 1) == Some(&'/') {
                        self.next_char(); // consume first '/'
                        self.next_char(); // consume second '/'
                        while let Some(c) = self.peek_char() {
                            if c == '\n' {
                                break;
                            }
                            self.next_char();
                        }
                    } else {
                        break;
                    }
                }
                Some('#') => {
                    // skip preprocessor or other comments starting with #
                    while let Some(c) = self.next_char() {
                        if c == '\n' {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    // Parse numbers (decimal, octal, or hex)
    fn lex_number(&mut self, first_digit: char) -> Token {
        let mut num: i64 = 0;

        if first_digit == '0' {
            if let Some('x') | Some('X') = self.peek_char() {
                self.next_char(); // consume x or X
                while let Some(c) = self.peek_char() {
                    if c.is_ascii_hexdigit() {
                        num = num * 16 + c.to_digit(16).unwrap() as i64;
                        self.next_char();
                    } else {
                        break;
                    }
                }
            } else {
                while let Some(c) = self.peek_char() {
                    if ('0'..='7').contains(&c) {
                        num = num * 8 + (c as u8 - b'0') as i64;
                        self.next_char();
                    } else {
                        break;
                    }
                }
            }
        } else {
            num = (first_digit as u8 - b'0') as i64;
            while let Some(c) = self.peek_char() {
                if c.is_ascii_digit() {
                    num = num * 10 + (c as u8 - b'0') as i64;
                    self.next_char();
                } else {
                    break;
                }
            }
        }

        Token::Num(num)
    }

    // Parse a char literal like 'a' or '\n'
    fn lex_char_literal(&mut self) -> Token {
        match self.next_char() {
            Some(c) => {
                let value = if c == '\\' {
                    match self.next_char() {
                        Some('n') => '\n',
                        Some('t') => '\t',
                        Some('r') => '\r',
                        Some('\\') => '\\',
                        Some('\'') => '\'',
                        Some('\"') => '\"',
                        Some(other) => other,
                        None => '\0',
                    }
                } else {
                    c
                };
                self.next_char(); // skip closing '
                Token::CharLiteral(value)
            }
            None => Token::Unknown('\''),
        }
    }

    // Parse a string literal like "hello"
    fn lex_string_literal(&mut self) -> Token {
        let mut s = String::new();

        while let Some(c) = self.next_char() {
            if c == '"' {
                return Token::StringLiteral(s);
            } else if c == '\\' {
                match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('r') => s.push('\r'),
                    Some('\\') => s.push('\\'),
                    Some('\'') => s.push('\''),
                    Some('"') => s.push('"'),
                    Some(other) => s.push(other),
                    None => break,
                }
            } else {
                s.push(c);
            }
        }

        Token::Unknown('"') // unterminated string
    }
}
