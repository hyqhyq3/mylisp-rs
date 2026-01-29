use crate::ast::Expr;
use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Parser { lexer, current_token }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match &self.current_token {
            Token::Number(n) => {
                let expr = Expr::Number(*n);
                self.advance();
                Ok(expr)
            }
            Token::Symbol(s) => {
                let expr = match s.as_str() {
                    "nil" => Expr::Nil,
                    _ => Expr::Symbol(s.clone()),
                };
                self.advance();
                Ok(expr)
            }
            Token::String(s) => {
                let expr = Expr::String(s.clone());
                self.advance();
                Ok(expr)
            }
            Token::Bool(b) => {
                let expr = Expr::Bool(*b);
                self.advance();
                Ok(expr)
            }
            Token::LParen => {
                self.advance();
                let mut list = Vec::new();
                while self.current_token != Token::RParen && self.current_token != Token::EOF {
                    list.push(self.parse_expr()?);
                }
                if self.current_token == Token::RParen {
                    self.advance();
                    Ok(Expr::List(list))
                } else {
                    Err("Unclosed parenthesis".to_string())
                }
            }
            Token::RParen => Err("Unexpected closing parenthesis".to_string()),
            Token::EOF => Err("Unexpected EOF".to_string()),
        }
    }
}
