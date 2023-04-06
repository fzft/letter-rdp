#![allow(unused)]


use std::borrow::Cow;
use std::fmt::{Display, format, Formatter};
use std::mem::discriminant;
use std::path::StripPrefixError;

use serde::{Deserialize, Serialize};
use serde::de::Unexpected::Option;

use crate::parser::Literal::{BoolLiteral, NullLiteral, NumberLiteral, StringLiteral};
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
 *  ; VariableStatement
 */
#[derive(Serialize, Deserialize, Debug)]
enum Statement {
    Expression(Box<ExpressionStatement>),
    Block(Box<BlockStatement>),
    Empty(Box<Empty>),
    Variable(Box<VariableStatement>),
    If(Box<IfStatement>),
}


#[derive(Serialize, Deserialize, Debug)]
struct Empty {
    t: String,
}

impl Empty {
    fn new() -> Self {
        Self {
            t: String::from("EmptyStatement")
        }
    }
}


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


#[derive(Serialize, Deserialize, Debug)]
struct VariableStatement {
    t: String,
    decls: Vec<Variable>,
}

impl VariableStatement {
    fn new() -> Self {
        Self {
            t: String::from("VariableStatement"),
            decls: Vec::new(),
        }
    }

    fn push(&mut self, v: Variable) {
        self.decls.push(v)
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct IfStatement {
    t: String,
    exp: Expression,
    consequent: Statement,
    alternate: Statement,
}

impl IfStatement {
    fn new(exp: Expression, consequent: Statement, alternate: Statement) -> Self {
        Self {
            t: String::from("IfStatement"),
            exp,
            consequent,
            alternate,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
enum Variable {
    Decl(VarDecl),
    Blank,
}

#[derive(Serialize, Deserialize, Debug)]
struct VarDecl {
    t: String,
    id: Expression,
    init: Expression,
}

impl VarDecl {
    fn new(id: Expression, init: Expression) -> Self {
        Self {
            t: String::from("VariableDeclaration"),
            id,
            init,
        }
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
    expression: Expression,
}

impl ExpressionStatement {
    fn new(expression: Expression) -> Self {
        Self {
            t: String::from("ExpressionStatement"),
            expression,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
enum Expression {
    Literal(Literal),
    BinaryExpression(Box<Binary>),
    AssignExpression(Box<Assign>),
    LogicalExpression(Box<Logical>),
    UnaryExpression(Box<Unary>),
    Ident(String),
    Blank,
}

#[derive(Serialize, Deserialize, Debug)]
struct Logical {
    t: String,
    op: String,
    left: Expression,
    right: Expression,
}

impl Logical {
    fn new(op: String, left: Expression, right: Expression) -> Self {
        Self {
            t: String::from("LogicalExpression"),
            left,
            right,
            op,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Unary {
    t: String,
    op: String,
    argument: Expression,
}

impl Unary {
    fn new(op: String, argument: Expression) -> Self {
        Self {
            t: String::from("UnaryExpression"),
            op,
            argument,
        }
    }
}


#[derive(Serialize, Deserialize, Debug)]
struct Binary {
    t: String,
    op: String,
    left: Expression,
    right: Expression,
}

impl Binary {
    fn new(op: String, left: Expression, right: Expression) -> Self {
        Self {
            t: String::from("BinaryExpression"),
            left,
            right,
            op,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Assign {
    t: String,
    op: String,
    left: Expression,
    right: Expression,
}

impl Assign {
    fn new(op: String, left: Expression, right: Expression) -> Self {
        Self {
            t: String::from("AssignExpression"),
            left,
            right,
            op,
        }
    }
}


#[derive(Serialize, Deserialize, Debug)]
enum Literal {
    StringLiteral(String),
    NumberLiteral(String),
    BoolLiteral(bool),
    NullLiteral(String),
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
        let body = self.stmt_list(Token::None)?;
        let mut p = Program::new();
        p.body = body;
        Ok(p)
    }


    fn stmt_list(&mut self, stop_lookahead: Token) -> Result<Vec<Statement>, String> {
        let mut body = Vec::<Statement>::new();
        let stmt = self.stmt()?;
        body.push(stmt);
        while self.lookahead != Token::EOF && self.lookahead != stop_lookahead {
            let stmt = self.stmt()?;
            body.push(stmt)
        }
        Ok(body)
    }

    fn stmt(&mut self) -> Result<Statement, String> {
        match self.lookahead {
            Token::OpenBrace => self.block(),
            Token::Semicolon => self.emp_stmt(),
            Token::If => self.if_stmt(),
            Token::Let => self.var_stmt(),
            _ => self.expression_stmt()
        }
    }
    fn var_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Let], |_| false)?;
        let declarations = self.var_decl_lst()?;
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(Statement::Variable(Box::new(declarations)))
    }

    fn emp_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(Statement::Empty(Box::new(Empty::new())))
    }

    fn if_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::If], |_| false)?;
        self.eat(&[Token::OpenParen], |_| false)?;
        let test = self.expression()?;
        self.eat(&[Token::CloseParen], |_| false)?;
        let consequent = self.stmt()?;
        let alternate: Statement = if self.lookahead != Token::EOF &&
            self.lookahead == Token::Else &&
            self.eat(&[Token::Else], |_| false).is_ok()
        {
            self.stmt()?
        } else {
            Statement::Empty(Box::new(Empty::new()))
        };

        Ok(Statement::If(Box::new(IfStatement::new(test, consequent, alternate))))
    }

    fn expression_stmt(&mut self) -> Result<Statement, String> {
        let exp = self.expression()?;
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(Statement::Expression(Box::new(ExpressionStatement::new(exp))))
    }

    fn expression(&mut self) -> Result<Expression, String> {
        self.assign_exp()
        // let l = self.literal()?;
        // self.eat(&[Token::Semicolon])?;
        // Ok(ExpressionStatement::new(Expression::Literal(l)))
    }

    fn block(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::OpenBrace], |_| false)?;

        let body = match self.lookahead {
            Token::CloseBrace => Statement::Empty(Box::new(Empty::new())),
            _ => {
                let stmts = self.stmt_list(Token::CloseBrace)?;
                let mut bs = BlockStatement::new();
                bs.body = stmts;
                Statement::Block(Box::new(bs))
            }
        };
        self.eat(&[Token::CloseBrace], |_| false)?;
        Ok(body)
    }


    fn assign_exp(&mut self) -> Result<Expression, String> {
        let left = self.logical_or()?;
        let operators = &[Token::SimpleAssign, Token::AddAssign, Token::MinusAssign, Token::MultiAssign, Token::DivAssign];
        if !operators.contains(&self.lookahead) {
            return Ok(left);
        }

        let operator = match self.eat(operators, |_| false)? {
            Token::SimpleAssign => String::from("="),
            Token::AddAssign => String::from("+="),
            Token::MinusAssign => String::from("-="),
            Token::MultiAssign => String::from("*="),
            Token::DivAssign => String::from("/="),
            _ => return Err(format!("Unexpected Assign operator"))
        };

        let left = self.check_valid_assignment_target(left)?;
        let right = self.assign_exp()?;
        Ok(Expression::AssignExpression(Box::new(Assign::new(operator, left, right))))
    }

    fn equal_exp(&mut self) -> Result<Expression, String> {
        let operators = &[Token::NotEqual, Token::DoubleEqual];
        self.binary_exp(operators, Self::rel_exp)
    }

    fn additive_exp(&mut self) -> Result<Expression, String> {
        let operators = &[Token::Plus, Token::Minus];
        self.binary_exp(operators, Self::multiplicative_exp)
    }

    /**
     * MultiplicativeExpression
     * : UnaryExpression
     * | MultiplicativeExpression
     */
    fn multiplicative_exp(&mut self) -> Result<Expression, String> {
        let operators = &[Token::Multiply, Token::Divide];
        self.binary_exp(operators, Self::unary_exp)
    }

    fn rel_exp(&mut self) -> Result<Expression, String> {
        let operators = &[Token::GreaterThan, Token::GreaterThanOrEqual, Token::LessThan, Token::LessThanOrEqual];
        self.binary_exp(operators, Self::additive_exp)
    }

    fn binary_exp<F>(&mut self, operators: &[Token], next_exp: F) -> Result<Expression, String>
        where
            F: Fn(&mut Self) -> Result<Expression, String>,
    {
        let mut left = next_exp(self)?;

        while operators.contains(&self.lookahead) {
            let operator = match self.eat(operators, |_| false)? {
                Token::Plus => String::from("+"),
                Token::Minus => String::from("-"),
                Token::Multiply => String::from("*"),
                Token::Divide => String::from("/"),
                Token::GreaterThan => String::from(">"),
                Token::GreaterThanOrEqual => String::from(">="),
                Token::LessThan => String::from("<"),
                Token::LessThanOrEqual => String::from("<="),
                Token::NotEqual => String::from("!="),
                Token::DoubleEqual => String::from("=="),
                _ => return Err(format!("Unexpected Binary operator"))
            };
            let right = next_exp(self)?;
            left = Expression::BinaryExpression(Box::new(Binary::new(operator, left, right)));
        }
        Ok(left)
    }

    fn logical_exp<F>(&mut self, operators: &[Token], next_exp: F) -> Result<Expression, String>
        where
            F: Fn(&mut Self) -> Result<Expression, String>,
    {
        let mut left = next_exp(self)?;

        while operators.contains(&self.lookahead) {
            let operator = match self.eat(operators, |_| false)? {
                Token::LogicalOr => String::from("||"),
                Token::LogicalAnd => String::from("&&"),
                _ => return Err(format!("Unexpected Logical operator"))
            };
            let right = next_exp(self)?;
            left = Expression::LogicalExpression(Box::new(Logical::new(operator, left, right)));
        }
        Ok(left)
    }

    fn logical_and(&mut self) -> Result<Expression, String> {
        let operators = &[Token::LogicalAnd];
        self.logical_exp(operators, Self::equal_exp)
    }

    fn logical_or(&mut self) -> Result<Expression, String> {
        let operators = &[Token::LogicalOr];
        self.logical_exp(operators, Self::logical_and)
    }

    /**
     * UnaryExpression
     * : LeftHandSideExpression
     * | ADDITIVE_OP UnaryExpression
     * | Logical_Not UnaryExpression
     * ;
     */
    fn unary_exp(&mut self) -> Result<Expression, String> {
        let op = match self.lookahead {
            Token::Plus | Token::Minus | Token::LogicalNot =>
                self.eat(&[Token::Plus, Token::Minus, Token::LogicalNot], |_| false)?,
            _ => Token::None
        };
        if op != Token::None {
            let operator = match op {
                Token::Plus => String::from("+"),
                Token::Minus => String::from("-"),
                Token::LogicalNot => String::from("!"),
                _ => return Err(format!("Unexpected unary operator"))
            };
            let argument = self.unary_exp()?;
            return Ok(Expression::UnaryExpression(Box::new(Unary::new(operator, argument))));
        }
        self.lhs_exp()
    }


    /**
     * PrimaryExpression
     * : Literal
     * | ParenthesizedExpression
     * | Ident
     * :
     */
    fn primary_exp(&mut self) -> Result<Expression, String> {
        if self.is_literal() {
            let l = self.literal()?;
            return Ok(Expression::Literal(l));
        }

        match self.lookahead {
            Token::OpenParen => self.paren_exp(),
            Token::Identifier(_) => self.ident(),
            _ => self.lhs_exp()
        }
    }


    fn lhs_exp(&mut self) -> Result<Expression, String> {
        self.primary_exp()
    }

    fn ident(&mut self) -> Result<Expression, String> {
        let ident = self.eat(&[], is_identifier)?;
        match ident {
            Token::Identifier(s) => Ok(Expression::Ident(s)),
            _ => Err(format!("Unexpected token, expected Identifier")),
        }
    }

    fn paren_exp(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::OpenParen], |_| false)?;
        let exp = self.expression()?;
        self.eat(&[Token::CloseParen], |_| false)?;
        Ok(exp)
    }


    fn literal(&mut self) -> Result<Literal, String> {
        let token = self.lookahead.clone();
        self.lookahead = self.tokenizer.next().unwrap_or_else(|| Token::EOF);
        match token {
            Token::Number(num) => Ok(NumberLiteral(num)),
            Token::StringLiteral(s) => Ok(StringLiteral(s)),
            Token::False => Ok(BoolLiteral(false)),
            Token::True => Ok(BoolLiteral(true)),
            Token::Null => Ok(NullLiteral(String::from("null"))),
            _ => Err(format!("Unexpected token, expected Literal"))
        }
    }

    fn is_literal(&self) -> bool {
        match self.lookahead {
            Token::Number(_) | Token::StringLiteral(_) | Token::Null | Token::True | Token::False => true,
            _ => false,
        }
    }


    // Expects a token of one of the given types
    fn eat<F>(&mut self, expected: &[Token], is_special_case: F) -> Result<Token, String>
        where
            F: Fn(&Token) -> bool,
    {
        let actual = &self.lookahead;

        if expected.iter().any(|token| discriminant(token) == discriminant(actual))
            || is_special_case(actual)
        {
            let matched_token = actual.clone();
            self.lookahead = self.tokenizer.next().unwrap_or_else(|| Token::EOF);
            Ok(matched_token)
        } else {
            Err(format!(
                "Unexpected token: {:?}, expected one of: {:?}",
                actual, expected
            ))
        }
    }


    fn check_valid_assignment_target(&self, exp: Expression) -> Result<Expression, String> {
        match exp {
            Expression::Ident(_) => Ok(exp),
            _ => Err(format!("Unexpected expression, expect Ident expression"))
        }
    }

    fn var_decl_lst(&mut self) -> Result<VariableStatement, String> {
        let mut decls = VariableStatement::new();

        loop {
            let mut var = self.var_decl()?;
            decls.push(var);

            if self.lookahead != Token::Comma || self.eat(&[Token::Comma], |_| false).is_err() {
                break;
            }
        }
        Ok(decls)
    }

    fn var_decl(&mut self) -> Result<Variable, String> {
        let id = self.ident()?;

        // OptVariableInitializer
        let init = if self.lookahead != Token::Semicolon && self.lookahead != Token::Comma {
            self.var_init()?
        } else {
            Expression::Blank
        };

        Ok(Variable::Decl(VarDecl::new(id, init)))
    }

    fn var_init(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::SimpleAssign], |_| false)?;
        self.assign_exp()
    }
}


fn is_identifier(token: &Token) -> bool {
    matches!(token, Token::Identifier(_))
}
