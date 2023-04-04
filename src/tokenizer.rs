#![allow(unused)]

use std::collections::HashMap;
use std::{fmt, vec};
use std::fmt::Formatter;
use std::str::Chars;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // NumericLiteral
    Number(String),
    Plus,
    Minus,
    Multiply,
    Divide,

    StringLiteral(String),

    Semicolon,

    // All skip token
    None,
    EOF,

    Unknown(char), // Add this line
}


struct PatternBox {
    f: Box<dyn Fn(&str) -> Token + Send + Sync>,
}

impl PatternBox {
    fn new<F>(f: F) -> Self
        where
            F: 'static + Fn(&str) -> Token + Send + Sync
    {
        Self { f: Box::new(f) }
    }

    fn execute(&self, input: &str) -> Token {
        (self.f)(input)
    }
}

lazy_static! {
    static ref SPEC: Vec<(&'static str, PatternBox)> = {
        let mut patterns = Vec::<(&'static str, PatternBox)>::new();
        // Float
        patterns.push(((r"^\d+(\.\d+)?", PatternBox::new(|s: &str| Token::Number(s.to_string())))));

        patterns.push((r"^;", PatternBox::new(|_: &str| Token::Semicolon)));
        patterns.push((r"^\+", PatternBox::new(|_: &str| Token::Plus)));
        patterns.push((r"^\-", PatternBox::new(|_: &str| Token::Minus)));
        patterns.push((r"^\*", PatternBox::new(|_: &str| Token::Multiply)));

        // patterns.insert(r"^/", PatternBox::new(|_: &str| Token::Divide));

          // Skip token
        patterns.push((r"^//.*", PatternBox::new(|_: &str| Token::None))); // single comments
        patterns.push((r"^\n", PatternBox::new(|_: &str| Token::None)));
        patterns.push((r"^\r", PatternBox::new(|_: &str| Token::None))); // return
        patterns.push((r"^\s+", PatternBox::new(|_: &str| Token::None))); // whitespace
        patterns.push((r"^/\*\*(.|\n)*?\*/", PatternBox::new(|_: &str| Token::None))); // multiple-line comments

         // String
        patterns.push((r#"^"([^"\\]|\\.)*""#, PatternBox::new(|s: &str| {
            Token::StringLiteral(s[1..s.len() - 1].to_string())
        })));

        // Unknown
        patterns.push((r"^.", PatternBox::new(|s: &str| Token::Unknown(s.chars().next().unwrap()))));
        patterns
    };
}


// pull a token from a stream
pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub(crate) fn new() -> Self {
        Self {
            input: "",
        }
    }

    pub(crate) fn init(&mut self, input: &'a str) {
        self.input = input;
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.input.is_empty() {
            for (pattern, token_constructor) in SPEC.iter() {
                if let Some(captures) = Regex::new(pattern).unwrap().captures(self.input) {
                    let token_str = captures.get(0).unwrap().as_str();

                    // Update the input by removing the matched token
                    self.input = &self.input[token_str.len()..];

                    // Return the corresponding token
                    let token = token_constructor.execute(token_str);
                    match token {
                        Token::None => break,
                        _ => {
                            return Some(token);
                        }
                    }
                }
            }
        }

        Some(Token::EOF)
    }
}