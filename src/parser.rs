#![allow(unused)]


use std::borrow::Cow;
use std::fmt::{Display, format, Formatter};
use std::mem::discriminant;
use std::path::StripPrefixError;

use serde::{Deserialize, Serialize};
use serde::de::Unexpected::Option;

use crate::parser::Expression::CallExpression;
use crate::parser::Literal::{BoolLiteral, NullLiteral, NumberLiteral, StringLiteral};
use crate::tokenizer::{Token, Tokenizer};
use crate::tokenizer::Token::{Number, While};

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
    Iteration(IterationStatement),
    Def(Box<FunctionDeclaration>),
    Return(ReturnStatement),
    Class(Box<ClassDeclaration>),
}

#[derive(Serialize, Deserialize, Debug)]
struct ClassDeclaration {
    t: String,
    id: Expression,
    super_clz: Expression,
    body: Statement,
}

impl ClassDeclaration {
    fn new(id: Expression, super_clz: Expression, body: Statement) -> Self {
        Self {
            t: String::from("ClassDeclaration"),
            id,
            super_clz,
            body,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct FunctionDeclaration {
    t: String,
    name: Expression,
    params: Vec<Expression>,
    body: Statement,
}

impl FunctionDeclaration {
    fn new(name: Expression, params: Vec<Expression>, body: Statement) -> Self {
        Self {
            t: String::from("FunctionDeclaration"),
            name,
            params,
            body,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct ReturnStatement {
    t: String,
    argument: Expression,
}

impl ReturnStatement {
    fn new(argument: Expression) -> Self {
        Self {
            t: String::from("ReturnStatement"),
            argument,
        }
    }
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
enum IterationStatement {
    While(Box<WhileStatement>),
    DoWhile(Box<DoWhileStatement>),
    For(Box<ForStatement>),
}

#[derive(Serialize, Deserialize, Debug)]
struct WhileStatement {
    t: String,
    test: Expression,
    body: Statement,
}

impl WhileStatement {
    fn new(test: Expression, body: Statement) -> Self {
        Self {
            t: String::from("WhileStatement"),
            test,
            body,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct DoWhileStatement {
    t: String,
    test: Expression,
    body: Statement,
}

impl DoWhileStatement {
    fn new(test: Expression, body: Statement) -> Self {
        Self {
            t: String::from("DoWhileStatement"),
            test,
            body,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct ForStatement {
    t: String,
    init: Statement,
    test: Expression,
    update: Expression,
    body: Statement,
}

impl ForStatement {
    fn new(init: Statement, test: Expression, update: Expression, body: Statement) -> Self {
        Self {
            t: String::from("ForStatement"),
            test,
            update,
            init,
            body,
        }
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
    MemberExpression(Box<Member>),
    Ident(String),
    CallExpression(Box<Call>),
    ThisExpression(This),
    SuperExpression(Super),
    NewExpression(Box<New>),
    Blank,
}


#[derive(Serialize, Deserialize, Debug)]
struct New {
    t: String,
    callee: Expression,
    args: Vec<Expression>,
}

impl New {
    fn new(callee: Expression, args: Vec<Expression>) -> Self {
        Self {
            t: String::from("NewExpression"),
            callee,
            args,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct This {
    t: String,
}

impl This {
    fn new() -> Self {
        Self {
            t: String::from("ThisExpression")
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Super {
    t: String,
}

impl Super {
    fn new() -> Self {
        Self {
            t: String::from("Super")
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Call {
    t: String,
    callee: Expression,
    arguments: Vec<Expression>,
}

impl Call {
    fn new(callee: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            t: String::from("CallExpression"),
            callee,
            arguments,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Member {
    t: String,
    computed: bool,
    obj: Expression,
    property: Expression,
}

impl Member {
    fn new(computed: bool, obj: Expression, property: Expression) -> Self {
        Self {
            t: String::from("MemberExpression"),
            obj,
            computed,
            property,
        }
    }
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
            Token::While | Token::For | Token::Do => self.iter_stmt(),
            Token::Def => self.func_decl(),
            Token::Return => self.ret_stmt(),
            Token::Class => self.clz_decl(),
            _ => self.expression_stmt()
        }
    }

    fn clz_decl(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Class], |_| false)?;
        let id = self.ident()?;
        let super_clz = if self.lookahead == Token::Extend {
            self.clz_ext()?
        } else {
            Expression::Blank
        };

        let body = self.block()?;

        Ok(Statement::Class(Box::new(ClassDeclaration::new(id, super_clz, body))))
    }

    fn clz_ext(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::Extend], |_| false)?;
        self.ident()
    }

    fn ret_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Return], |_| false)?;
        let argument = if self.lookahead != Token::Semicolon {
            self.expression()?
        } else {
            Expression::Blank
        };
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(Statement::Return(ReturnStatement::new(argument)))
    }

    fn func_decl(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Def], |_| false)?;
        let name = self.ident()?;
        self.eat(&[Token::OpenParen], |_| false)?;
        let params = if self.lookahead != Token::CloseParen {
            self.formal_param_lst()?
        } else {
            Vec::new()
        };
        self.eat(&[Token::CloseParen], |_| false)?;
        let body = self.block()?;
        Ok(Statement::Def(Box::new(FunctionDeclaration::new(name, params, body))))
    }

    fn formal_param_lst(&mut self) -> Result<Vec<Expression>, String> {
        let mut params = Vec::<Expression>::new();
        loop {
            let ident = self.ident()?;
            params.push(ident);

            if self.lookahead != Token::Comma || self.eat(&[Token::Comma], |_| false).is_err() {
                break;
            }
        }
        Ok(params)
    }

    fn iter_stmt(&mut self) -> Result<Statement, String> {
        match self.lookahead {
            Token::Do => self.do_stmt(),
            Token::While => self.while_stmt(),
            Token::For => self.for_stmt(),
            _ => Err(format!("Unexpected iter operator"))
        }
    }

    fn do_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Do], |_| false)?;
        let body = self.stmt()?;
        self.eat(&[Token::While], |_| false)?;
        self.eat(&[Token::OpenParen], |_| false)?;
        let test = self.expression()?;
        self.eat(&[Token::CloseParen], |_| false)?;
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(Statement::Iteration(IterationStatement::DoWhile(Box::new(DoWhileStatement::new(test, body)))))
    }

    fn while_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::While], |_| false)?;
        self.eat(&[Token::OpenParen], |_| false)?;
        let test = self.expression()?;
        self.eat(&[Token::CloseParen], |_| false)?;
        let body = self.stmt()?;
        Ok(Statement::Iteration(IterationStatement::While(Box::new(WhileStatement::new(test, body)))))
    }
    fn for_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::For], |_| false)?;
        self.eat(&[Token::OpenParen], |_| false)?;
        let init = if self.lookahead != Token::Semicolon {
            self.for_init_stmt()?
        } else {
            Statement::Empty(Box::new(Empty::new()))
        };
        self.eat(&[Token::Semicolon], |_| false)?;

        let test = if self.lookahead != Token::Semicolon {
            self.expression()?
        } else {
            Expression::Blank
        };
        self.eat(&[Token::Semicolon], |_| false)?;
        let update = if self.lookahead != Token::CloseParen {
            self.expression()?
        } else {
            Expression::Blank
        };
        self.eat(&[Token::CloseParen], |_| false)?;
        let body = self.stmt()?;

        Ok(Statement::Iteration(IterationStatement::For(Box::new(ForStatement::new(init, test, update, body)))))
    }

    /**
     * ForStatementInit
     * : VariableStatementInit
     * | Expression
     */
    fn for_init_stmt(&mut self) -> Result<Statement, String> {
        match self.lookahead {
            Token::Let => self.var_init_stmt(),
            _ => {
                let exp = self.expression()?;
                Ok(Statement::Expression(Box::new(ExpressionStatement::new(exp))))
            }
        }
    }

    fn var_init_stmt(&mut self) -> Result<Statement, String> {
        self.eat(&[Token::Let], |_| false)?;
        let declarations = self.var_decl_lst()?;
        Ok(Statement::Variable(Box::new(declarations)))
    }

    fn var_stmt(&mut self) -> Result<Statement, String> {
        let variable_stmt = self.var_init_stmt()?;
        self.eat(&[Token::Semicolon], |_| false)?;
        Ok(variable_stmt)
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
     * | ThisExpression
     * | NewExpression
     * :
     */
    fn primary_exp(&mut self) -> Result<Expression, String> {
        if self.is_literal() {
            let l = self.literal()?;
            return Ok(Expression::Literal(l));
        }

        return match self.lookahead {
            Token::OpenParen => self.paren_exp(),
            Token::Identifier(_) => self.ident(),
            Token::This => self.this_exp(),
            Token::New => self.new_exp(),
            _ => self.lhs_exp()
        };
    }

    fn new_exp(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::New], |_| false)?;
        let callee = self.member_exp()?;
        let args = self.args()?;
        Ok(Expression::NewExpression(Box::new(New::new(callee, args))))
    }

    fn this_exp(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::This], |_| false)?;
        Ok(Expression::ThisExpression(This::new()))
    }

    /**
     * LeftHandSideExpression
     * : CallMemberExpression
     * :
     */
    fn lhs_exp(&mut self) -> Result<Expression, String> {
        self.call_member_exp()
    }

    /**
     * CallMemberExpression
     * : MemberExpression
     * : CallExpression
     * :
     */
    fn call_member_exp(&mut self) -> Result<Expression, String> {
        if self.lookahead == Token::Super {
            let su = self.super_call()?;
            return self.call_exp(su);
        }
        let member = self.member_exp()?;
        if self.lookahead == Token::OpenParen {
            return self.call_exp(member);
        }

        Ok(member)
    }
    /**
     * Callee
     * : MemberExpression
     * | CallExpression
     */
    fn call_exp(&mut self, callee: Expression) -> Result<Expression, String> {
        let mut arguments = self.args()?;
        let mut call_exp = Expression::CallExpression(Box::new(Call::new(callee, arguments)));
        if self.lookahead == Token::OpenParen {
            call_exp = self.call_exp(call_exp)?;
        }

        Ok(call_exp)
    }

    fn super_call(&mut self) -> Result<Expression, String> {
        self.eat(&[Token::Super], |_| false)?;
        Ok(Expression::SuperExpression(Super::new()))
    }

    /**
     * Arguments
     *  : ('OptArgumentList')
     *
     */
    fn args(&mut self) -> Result<Vec<Expression>, String> {
        self.eat(&[Token::OpenParen], |_| false)?;
        let arg_list = if self.lookahead != Token::CloseParen {
            self.argument_lst()?
        } else {
            Vec::new()
        };
        self.eat(&[Token::CloseParen], |_| false)?;
        Ok(arg_list)
    }

    fn argument_lst(&mut self) -> Result<Vec<Expression>, String> {
        let mut argument_list = Vec::<Expression>::new();
        loop {
            let var = self.assign_exp()?;
            argument_list.push(var);

            if self.lookahead != Token::Comma || self.eat(&[Token::Comma], |_| false).is_err() {
                break;
            }
        }
        Ok(argument_list)
    }

    /**
     * LeftHandSideExpression
     * : PrimaryExpression
     * : MemberExpression '.' Ident
     * ; MemberExpression '['Expression']'
     */
    fn member_exp(&mut self) -> Result<Expression, String> {
        let mut obj = self.primary_exp()?;
        while self.lookahead == Token::Dot || self.lookahead == Token::OpenSqrBracket {
            match self.lookahead {
                Token::Dot => {
                    self.eat(&[Token::Dot], |_| false)?;
                    let property = self.ident()?;
                    obj = Expression::MemberExpression(Box::new(Member::new(false, obj, property)));
                }
                Token::OpenSqrBracket => {
                    self.eat(&[Token::OpenSqrBracket], |_| false)?;
                    let property = self.expression()?;
                    self.eat(&[Token::CloseSqrBracket], |_| false)?;
                    obj = Expression::MemberExpression(Box::new(Member::new(true, obj, property)));
                }
                _ => return Err(format!("Unexpected member expression"))
            }
        }
        Ok(obj)
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
            Expression::MemberExpression(_) => Ok(exp),
            _ => Err(format!("Unexpected left-hand side in assignment expression"))
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
