#![allow(unused)]

use std::{fmt, vec};
use std::collections::HashMap;
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

    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    NotEqual,
    DoubleEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,

    StringLiteral(String),

    Semicolon,
    Comma,

    // All skip token
    None,
    EOF,

    Unknown(char), // Add this line

    OpenBrace,
    CloseBrace,
    OpenSqrBracket,
    CloseSqrBracket,
    OpenParen,
    CloseParen,

    Dot,

    SimpleAssign,
    AddAssign,
    MinusAssign,
    MultiAssign,
    DivAssign,

    Identifier(String),

    // Keywords:
    Let,
    If,
    Else,
    True,
    False,
    Null,
    While,
    Do,
    For,
    Def,
    Return,
    Class,
    Extend,
    New,
    This,
    Super
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

        // Keywords
        patterns.push((r"^\blet\b", PatternBox::new(|_: &str| Token::Let)));
        patterns.push((r"^\bif\b", PatternBox::new(|_: &str| Token::If)));
        patterns.push((r"^\belse\b", PatternBox::new(|_: &str| Token::Else)));
        patterns.push((r"^\btrue\b", PatternBox::new(|_: &str| Token::True)));
        patterns.push((r"^\bfalse\b", PatternBox::new(|_: &str| Token::False)));
        patterns.push((r"^\bnull\b", PatternBox::new(|_: &str| Token::Null)));
        patterns.push((r"^\bdo\b", PatternBox::new(|_: &str| Token::Do)));
        patterns.push((r"^\bfor\b", PatternBox::new(|_: &str| Token::For)));
        patterns.push((r"^\bwhile\b", PatternBox::new(|_: &str| Token::While)));
        patterns.push((r"^\bdef\b", PatternBox::new(|_: &str| Token::Def)));
        patterns.push((r"^\breturn\b", PatternBox::new(|_: &str| Token::Return)));
        patterns.push((r"^\bclass\b", PatternBox::new(|_: &str| Token::Class)));
        patterns.push((r"^\bsuper\b", PatternBox::new(|_: &str| Token::Super)));
        patterns.push((r"^\bnew\b", PatternBox::new(|_: &str| Token::New)));
        patterns.push((r"^\bthis\b", PatternBox::new(|_: &str| Token::This)));
        patterns.push((r"^\bextend\b", PatternBox::new(|_: &str| Token::Extend)));


        patterns.push((r"^\+=", PatternBox::new(|_: &str| Token::AddAssign)));
        patterns.push((r"^\-=", PatternBox::new(|_: &str| Token::MinusAssign)));
        patterns.push((r"^\+", PatternBox::new(|_: &str| Token::Plus)));
        patterns.push((r"^&&", PatternBox::new(|_: &str| Token::LogicalAnd)));
        patterns.push((r"^\|\|", PatternBox::new(|_: &str| Token::LogicalOr)));
        patterns.push((r"^!", PatternBox::new(|_: &str| Token::LogicalNot)));

        patterns.push((r"^\w+", PatternBox::new(|s: &str| Token::Identifier(s.to_string()))));

          // Skip token
        patterns.push((r"^//.*", PatternBox::new(|_: &str| Token::None))); // single comments
        patterns.push((r"^\n", PatternBox::new(|_: &str| Token::None)));
        patterns.push((r"^\r", PatternBox::new(|_: &str| Token::None))); // return
        patterns.push((r"^\s+", PatternBox::new(|_: &str| Token::None))); // whitespace
        patterns.push((r"^/\*\*(.|\n)*?\*/", PatternBox::new(|_: &str| Token::None))); // multiple-line comments

        patterns.push((r"^\{", PatternBox::new(|_: &str| Token::OpenBrace)));
        patterns.push((r"^\}", PatternBox::new(|_: &str| Token::CloseBrace)));
        patterns.push((r"^\[", PatternBox::new(|_: &str| Token::OpenSqrBracket)));
        patterns.push((r"^\]", PatternBox::new(|_: &str| Token::CloseSqrBracket)));

        patterns.push((r"^\.", PatternBox::new(|_: &str| Token::Dot)));

        patterns.push((r"^\(", PatternBox::new(|_: &str| Token::OpenParen)));
        patterns.push((r"^\)", PatternBox::new(|_: &str| Token::CloseParen)));

        patterns.push((r"^;", PatternBox::new(|_: &str| Token::Semicolon)));
        patterns.push((r"^,", PatternBox::new(|_: &str| Token::Comma)));
        patterns.push((r"^\-", PatternBox::new(|_: &str| Token::Minus)));

        patterns.push((r"^>", PatternBox::new(|_: &str| Token::GreaterThan)));
        patterns.push((r"^<", PatternBox::new(|_: &str| Token::LessThan)));
        patterns.push((r"^>=", PatternBox::new(|_: &str| Token::GreaterThanOrEqual)));
        patterns.push((r"^<=", PatternBox::new(|_: &str| Token::LessThan)));
        patterns.push((r"^!=", PatternBox::new(|_: &str| Token::NotEqual)));
        patterns.push((r"^==", PatternBox::new(|_: &str| Token::DoubleEqual)));




        // Assign
        patterns.push((r"^\*=", PatternBox::new(|_: &str| Token::MultiAssign)));
        patterns.push((r"^/=", PatternBox::new(|_: &str| Token::DivAssign)));

        patterns.push((r"^=", PatternBox::new(|_: &str| Token::SimpleAssign)));

        patterns.push((r"^/", PatternBox::new(|_: &str| Token::Divide)));
        patterns.push((r"^\*", PatternBox::new(|_: &str| Token::Multiply)));



         // String
        patterns.push((r#"^(?:"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*')"#, PatternBox::new(|s: &str| {
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
                            dbg!(token.clone());
                            return Some(token);
                        }
                    }
                }
            }
        }

        Some(Token::EOF)
    }
}