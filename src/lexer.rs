#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    Number(f64),
    Symbol(String),
    String(String),
    Bool(bool),
    EOF,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            let c = self.input[self.pos];
            self.pos += 1;
            Some(c)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else if c == ';' {
                // 跳过注释到行尾
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn read_number(&mut self, first: char) -> f64 {
        let mut num_str = String::from(first);
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() || c == '.' {
                num_str.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        num_str.parse().unwrap_or(0.0)
    }

    fn read_string(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.advance() {
            if c == '"' {
                break;
            }
            if c == '\\' {
                if let Some(next) = self.advance() {
                    match next {
                        'n' => s.push('\n'),
                        't' => s.push('\t'),
                        'r' => s.push('\r'),
                        '\\' => s.push('\\'),
                        '"' => s.push('"'),
                        _ => s.push(next),
                    }
                }
            } else {
                s.push(c);
            }
        }
        s
    }

    fn read_symbol(&mut self, first: char) -> String {
        let mut sym_str = String::from(first);
        while let Some(c) = self.peek() {
            if !c.is_whitespace() && c != '(' && c != ')' {
                sym_str.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        sym_str
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.advance() {
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('"') => Token::String(self.read_string()),
            Some(c) if c.is_ascii_digit() || c == '-' || c == '+' => {
                if c == '-' || c == '+' {
                    if let Some(next) = self.peek() {
                        if next.is_ascii_digit() {
                            let num = self.read_number(c);
                            Token::Number(if c == '-' { -num } else { num })
                        } else {
                            Token::Symbol(self.read_symbol(c))
                        }
                    } else {
                        Token::Symbol(c.to_string())
                    }
                } else {
                    Token::Number(self.read_number(c))
                }
            }
            Some('#') => {
                if let Some(c) = self.advance() {
                    match c {
                        't' => Token::Bool(true),
                        'f' => Token::Bool(false),
                        _ => Token::Symbol(format!("#{}", c)),
                    }
                } else {
                    Token::Symbol("#".to_string())
                }
            }
            Some(c) => Token::Symbol(self.read_symbol(c)),
            None => Token::EOF,
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token == Token::EOF {
            None
        } else {
            Some(token)
        }
    }
}
