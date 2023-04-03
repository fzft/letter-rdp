#![allow(unused)]

use crate::parser::Token::Number;

#[non_exhaustive]
#[derive(Debug, PartialEq)]
enum Token<'a> {
    // NumericLiteral
    Number(&'a str)
}


struct Parser<'a> {
    input: &'a str
}

impl <'a> Parser <'a> {
    fn new(input: &'a str) -> Self {
        Self{
            input
        }
    }

    fn parse(&self) -> Token {
       Number(self.input)
    }
}




mod tests {
    use super::*;

    #[test]
    fn test_number() {
        let num = "11";
        let p = Parser::new(num);
        let ast = p.parse();
        assert_eq!(ast, Number(num))
    }
}