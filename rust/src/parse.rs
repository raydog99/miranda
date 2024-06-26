use std::str::Chars;

#[derive(Debug)]
enum Expr {
    Var(String),
    IntLit(i64),
    StringLit(String),
    App(Box<Expr>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    Let(Vec<(String, Box<Expr>)>, Box<Expr>),
}

#[derive(Debug)]
struct Decl {
    name: String,
    params: Vec<String>,
    body: Box<Expr>,
}

#[derive(Debug)]
struct Script {
    declarations: Vec<Decl>,
}

struct Parser<'a> {
    input: Chars<'a>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            input: input.chars(),
        }
    }

    fn parse(&mut self) -> Result<Script, String> {
        let mut script = Script {
            declarations: Vec::new(),
        };

        while let Some(decl) = self.parse_decl() {
            script.declarations.push(decl?);
        }

        Ok(script)
    }

    fn parse_decl(&mut self) -> Option<Result<Decl, String>> {
        let name = self.parse_identifier()?;
        let mut params = Vec::new();

        while self.peek() != Some('=') {
            params.push(self.parse_identifier()?);
        }

        self.consume('=')?;
        let body = self.parse_expr()?;
        self.consume(';')?;

        Some(Ok(Decl {
            name,
            params,
            body: Box::new(body),
        }))
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        if let Some(c) = self.peek() {
            if c.is_alphabetic() {
                return Ok(Expr::Var(self.parse_identifier().unwrap()));
            }
        }
        Err("Unsupported expression".to_string())
    }

    fn parse_identifier(&mut self) -> Option<String> {
        let mut ident = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphabetic() {
                ident.push(self.input.next()?);
            } else {
                break;
            }
        }
        if ident.is_empty() {
            None
        } else {
            Some(ident)
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.input.clone().next()
    }

    fn consume(&mut self, expected: char) -> Result<(), String> {
        if self.input.next() == Some(expected) {
            Ok(())
        } else {
            Err(format!("Expected '{}', got unexpected character", expected))
        }
    }
}