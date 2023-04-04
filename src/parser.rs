#![allow(unused)]


use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::tokenizer::{Token, Tokenizer};
use crate::tokenizer::Token::Number;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    p_type: String,
    body: Vec<Literal>,
}

impl Program {
    fn new() -> Self {
        Self {
            p_type: String::from("program"),
            body: Vec::new(),
        }
    }

    fn push(&mut self, l: Literal) {
        self.body.push(l)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pretty_json = serde_json::to_string_pretty(self).unwrap();
        write!(f, "{}", pretty_json)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Literal {
    t_literal: String,
    value: String,
}

impl Literal {
    fn new(t_literal: String, value: String) -> Self {
        Self {
            t_literal,
            value,
        }
    }
}

pub struct Parser<'a> {
    input: &'a str,
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Self {
            input: "",
            tokenizer: Tokenizer::new(),
        }
    }

    // Parses a string into an AST
    pub fn parse(&mut self, input: &'a str) -> Program {
        self.input = input;
        self.tokenizer.init(input);

        // Prime the tokenizer to obtain the first
        // token which is our lookahead. the lookahead is
        // used for predictive parsing

        // Parse recursively starting from the main
        // entry point, the Program.
        self.program()
    }

    // fn statement_list(&mut self) -> Vec<Statement> {
    //     let mut statement_list = Vec::<Statement>::new();
    //     while self.lookahead != None {
    //         statement_list.push(Statement {})
    //     }
    //     return statement_list;
    // }

    fn program(&mut self) -> Program {
        let mut p = Program::new();

        loop {
            let lookahead = self.tokenizer.next();
            match lookahead {
                Some(Token::Number(num)) => {
                    p.push(Literal::new(String::from("Number"), num))
                }
                Some(Token::StringLiteral(s)) =>
                    p.push(Literal::new(String::from("String"), s)),
                Some(Token::EOF) => break,
                Some(Token::Unknown(ch)) => {
                    // Handle unexpected characters, e.g., print a warning or ignore them
                    println!("Warning: Unexpected character '{}'", ch);
                    continue;
                }
                _ => { }
            }
        }
        p
    }

    // Expects a token of a given type
    fn eat(&mut self, expected: &Token, actual: &Token) -> bool {
        expected == actual
    }
}

