#![allow(unused)]


use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};
use serde::de::Unexpected::Option;

use crate::parser::Literal::{NumberLiteral, StringLiteral};
use crate::tokenizer::{Token, Tokenizer};
use crate::tokenizer::Token::Number;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    t: String,
    body: Vec<Statement>,
}

impl Program {
    fn new() -> Self {
        Self {
            t: String::from("program"),
            body: Vec::new(),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pretty_json = serde_json::to_string_pretty(self).unwrap();
        write!(f, "{}", pretty_json)
    }
}

/**
 *Statement
 *  : ExpressionStatement
 *  | BlockStatement
 *  ;
 */
#[derive(Serialize, Deserialize, Debug)]
enum Statement {
    Expression(Box<ExpressionStatement>),
    Block(Box<BlockStatement>),
    Blank(Box<Blank>),
}

#[derive(Serialize, Deserialize, Debug)]
struct Blank {}

/**
 * BlockStatement
 * : '{' OptStatement '}'
 * ;
 */
#[derive(Serialize, Deserialize, Debug)]
struct BlockStatement {
    t: String,
    body: Vec<Statement>,
}

impl BlockStatement {
    fn new() -> Self {
        Self {
            t: String::from("BlockStatement"),
            body: Vec::new(),
        }
    }

    fn push(&mut self, stmt: Statement) {
        self.body.push(stmt);
    }
}


/**
 *  ExpressionStatement
 *  | Expression
 *  ;
 */
#[derive(Serialize, Deserialize, Debug)]
struct ExpressionStatement {
    t: String,
    expression: Literal,
}

impl ExpressionStatement {
    fn new(expression: Literal) -> Self {
        Self {
            t: String::from("ExpressionStatement"),
            expression,
        }
    }
}


#[derive(Serialize, Deserialize, Debug)]
enum Literal {
    StringLiteral(String),
    NumberLiteral(String),
    None,
}


pub struct Parser<'a> {
    input: &'a str,
    tokenizer: Tokenizer<'a>,
    lookahead: Token,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Self {
            input: "",
            tokenizer: Tokenizer::new(),
            lookahead: Token::None,
        }
    }

    // Parses a string into an AST
    pub fn parse(&mut self, input: &'a str) -> Result<Program, String> {
        self.input = input;
        self.tokenizer.init(input);

        // Prime the tokenizer to obtain the first
        // token which is our lookahead. the lookahead is
        // used for predictive parsing
        self.lookahead = self.tokenizer.next().unwrap();

        // Parse recursively starting from the main
        // entry point, the Program.
        self.program()
    }

    fn program(&mut self) -> Result<Program, String> {
        let body = self.statement_list(Token::None)?;
        let mut p = Program::new();
        p.body = body;
        Ok(p)
    }


    fn statement_list(&mut self, stop_lookahead: Token) -> Result<Vec<Statement>, String> {
        let mut body = Vec::<Statement>::new();
        let stmt = self.statement()?;
        body.push(stmt);
        while self.lookahead != Token::EOF && self.lookahead != stop_lookahead {
            let stmt = self.statement()?;
            body.push(stmt)
        }
        Ok(body)
    }

    fn statement(&mut self) -> Result<Statement, String> {
        match self.lookahead {
            Token::OpenBrace => self.block(),
            _ => {
                let exp = self.expression()?;
                Ok(Statement::Expression(Box::new(exp)))
            }
        }
    }

    fn expression(&mut self) -> Result<ExpressionStatement, String> {
        let expression = self.literal()?;
        self.eat(&Token::Semicolon)?;
        Ok(ExpressionStatement::new(expression))
    }

    fn block(&mut self) -> Result<Statement, String> {
        self.eat(&Token::OpenBrace)?;

        let body = match self.lookahead {
            Token::CloseBrace => Statement::Blank(Box::new(Blank {})),
            _ => {
                let stmts = self.statement_list(Token::CloseBrace)?;
                let mut bs = BlockStatement::new();
                bs.body = stmts;
                Statement::Block(Box::new(bs))
            }
        };
        self.eat(&Token::CloseBrace)?;
        Ok(body)
    }


    fn literal(&mut self) -> Result<Literal, String> {
        let token = self.lookahead.clone();
        self.lookahead = self.tokenizer.next().unwrap_or_else(|| Token::EOF);
        match token {
            Token::Number(num) => Ok(NumberLiteral(num)),
            Token::StringLiteral(s) => Ok(StringLiteral(s)),
            _ => Err(format!("Unexpected token, expected Literal"))
        }
    }


    // Expects a token of a given type
    fn eat(&mut self, expected: &Token) -> Result<Token, String> {
        // let actual = self.tokenizer.next().ok_or_else(|| format!("Unexpected end of input, expected: {:?}", expected))?;
        let actual = &self.lookahead;
        if actual == expected {
            self.lookahead = self.tokenizer.next().unwrap_or_else(|| Token::EOF);
            Ok(expected.clone())
        } else {
            Err(format!("Unexpected token: {:?}, expected: {:?}", actual, expected))
        }
    }
}

