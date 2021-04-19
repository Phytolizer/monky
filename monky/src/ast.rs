use itertools::join;
use monky_test_macros::IsAs;

use std::fmt::Display;

use crate::token::Token;

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub enum Node {
    Expression(Expression),
    Statement(Statement),
    Program(Program),
    IntegerLiteral(IntegerLiteral),
    ExpressionStatement(ExpressionStatement),
}

impl TokenLiteral for Node {
    fn token_literal(&self) -> String {
        match self {
            Node::Expression(e) => e.token_literal(),
            Node::Statement(s) => s.token_literal(),
            Node::Program(p) => p.token_literal(),
            Node::IntegerLiteral(i) => i.token_literal(),
            Node::ExpressionStatement(e) => e.token_literal(),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Node::Expression(n) => n.to_string(),
                Node::Statement(n) => n.to_string(),
                Node::Program(n) => n.to_string(),
                Node::IntegerLiteral(n) => n.to_string(),
                Node::ExpressionStatement(n) => n.to_string(),
            }
        )
    }
}

#[derive(Debug, IsAs)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(Boolean),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
}

impl TokenLiteral for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(e) => e.token_literal(),
            Self::IntegerLiteral(i) => i.token_literal(),
            Self::Prefix(p) => p.token_literal(),
            Self::Infix(i) => i.token_literal(),
            Self::Boolean(b) => b.token_literal(),
            Self::If(i) => i.token_literal(),
            Self::Function(f) => f.token_literal(),
            Self::Call(c) => c.token_literal(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Identifier(e) => e.to_string(),
                Self::IntegerLiteral(i) => i.to_string(),
                Self::Prefix(p) => p.to_string(),
                Self::Infix(i) => i.to_string(),
                Self::Boolean(b) => b.to_string(),
                Self::If(i) => i.to_string(),
                Self::Function(f) => f.to_string(),
                Self::Call(c) => c.to_string(),
            }
        )
    }
}

#[derive(Debug, IsAs)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl TokenLiteral for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(s) => s.token_literal(),
            Self::Return(s) => s.token_literal(),
            Self::Expression(s) => s.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Statement::Let(s) => s.to_string(),
                Statement::Return(s) => s.to_string(),
                Statement::Expression(s) => s.to_string(),
            }
        )
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl TokenLiteral for Program {
    fn token_literal(&self) -> String {
        self.statements
            .get(0)
            .map(|stmt| stmt.token_literal())
            .unwrap_or_else(String::new)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

pub trait GenericNode<'a>: Display {
    fn name(&self) -> &'static str;
    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>>;
}

impl<'a> GenericNode<'a> for Statement {
    fn name(&self) -> &'static str {
        "Statement"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        match self {
            Statement::Let(s) => s.children(),
            Statement::Return(s) => s.children(),
            Statement::Expression(s) => s.children(),
        }
    }
}

impl<'a> GenericNode<'a> for LetStatement {
    fn name(&self) -> &'static str {
        "LetStatement"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        if let Some(value) = &self.value {
            vec![&self.name, value]
        } else {
            vec![&self.name]
        }
    }
}

impl<'a> GenericNode<'a> for Identifier {
    fn name(&self) -> &'static str {
        "Identifier"
    }

    fn children(&self) -> Vec<&'a dyn GenericNode<'a>> {
        vec![]
    }
}

impl<'a> GenericNode<'a> for Expression {
    fn name(&self) -> &'static str {
        match self {
            Expression::Identifier(_) => "Identifier",
            Expression::IntegerLiteral(_) => "IntegerLiteral",
            Expression::Prefix(_) => "Prefix",
            Expression::Infix(_) => "Infix",
            Expression::Boolean(_) => "Boolean",
            Expression::If(_) => "If",
            Expression::Function(_) => "Function",
            Expression::Call(_) => "Call",
        }
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        match self {
            Expression::Identifier(e) => e.children(),
            Expression::IntegerLiteral(e) => e.children(),
            Expression::Prefix(e) => e.children(),
            Expression::Infix(e) => e.children(),
            Expression::Boolean(e) => e.children(),
            Expression::If(e) => e.children(),
            Expression::Function(e) => e.children(),
            Expression::Call(e) => e.children(),
        }
    }
}

impl<'a> GenericNode<'a> for Boolean {
    fn name(&self) -> &'static str {
        "Boolean"
    }

    fn children(&self) -> Vec<&'a dyn GenericNode<'a>> {
        vec![]
    }
}

impl<'a> GenericNode<'a> for IntegerLiteral {
    fn name(&self) -> &'static str {
        "IntegerLiteral"
    }

    fn children(&self) -> Vec<&'a dyn GenericNode<'a>> {
        vec![]
    }
}

impl<'a> GenericNode<'a> for PrefixExpression {
    fn name(&self) -> &'static str {
        "PrefixExpression"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        vec![self.right.as_ref()]
    }
}

impl<'a> GenericNode<'a> for InfixExpression {
    fn name(&self) -> &'static str {
        "InfixExpression"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        vec![self.left.as_ref(), self.right.as_ref()]
    }
}

impl<'a> GenericNode<'a> for ReturnStatement {
    fn name(&self) -> &'static str {
        "ReturnStatement"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        if let Some(value) = &self.value {
            vec![value]
        } else {
            vec![]
        }
    }
}

impl<'a> GenericNode<'a> for ExpressionStatement {
    fn name(&self) -> &'static str {
        "ExpressionStatement"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        if let Some(expression) = self.expression.as_ref() {
            vec![expression]
        } else {
            vec![]
        }
    }
}

fn build_tree<'a>(builder: &mut ptree::TreeBuilder, children: Vec<&'a dyn GenericNode<'a>>) {
    for child in children {
        let mut s = child.name().to_string();
        s.push_str(&format!(r#" "{}""#, child));
        builder.begin_child(s);
        build_tree(builder, child.children());
        builder.end_child();
    }
}

impl Program {
    pub fn pretty_print(&self) -> String {
        let mut builder = ptree::TreeBuilder::new("Program".into());
        let mut v = Vec::<&dyn GenericNode>::new();
        for stmt in self.statements.iter() {
            v.push(stmt);
        }
        build_tree(&mut builder, v);
        let mut output = Vec::<u8>::new();
        ptree::write_tree(&builder.build(), &mut output).unwrap();
        String::from_utf8(output).unwrap()
    }
}

#[derive(Debug, Default)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl TokenLiteral for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name.to_string(),
            self.value
                .as_ref()
                .map(|e| e.to_string())
                .unwrap_or_else(String::new)
        )
    }
}

#[derive(Debug, Default)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Expression>,
}

impl TokenLiteral for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {};",
            self.token_literal(),
            self.value
                .as_ref()
                .map(|v| v.to_string())
                .unwrap_or_else(String::new)
        )
    }
}

#[derive(Debug, Default)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl TokenLiteral for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.expression
                .as_ref()
                .map(|e| e.to_string())
                .unwrap_or_default()
        )
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl TokenLiteral for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", join(self.statements.iter(), ""))
    }
}

#[derive(Debug, Default)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl TokenLiteral for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl TokenLiteral for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl TokenLiteral for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl TokenLiteral for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl TokenLiteral for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl TokenLiteral for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if{} {}{}",
            self.condition,
            self.consequence,
            self.alternative
                .as_ref()
                .map(|c| format!(" else {}", c))
                .unwrap_or_default()
        )
    }
}

impl<'a> GenericNode<'a> for IfExpression {
    fn name(&self) -> &'static str {
        "IfExpression"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        if let Some(alternative) = &self.alternative {
            vec![
                self.condition.as_ref(),
                self.consequence.as_ref(),
                alternative.as_ref(),
            ]
        } else {
            vec![self.condition.as_ref(), self.consequence.as_ref()]
        }
    }
}

impl<'a> GenericNode<'a> for BlockStatement {
    fn name(&self) -> &'static str {
        "BlockStatement"
    }

    fn children(&self) -> Vec<&dyn GenericNode<'_>> {
        let mut c = Vec::<&dyn GenericNode<'_>>::new();
        for child in &self.statements {
            c.push(child);
        }
        c
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Box<BlockStatement>,
}

impl TokenLiteral for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}){}",
            self.token_literal(),
            join(self.parameters.iter(), ", "),
            self.body
        )
    }
}

impl<'a> GenericNode<'a> for FunctionLiteral {
    fn name(&self) -> &'static str {
        "FunctionLiteral"
    }

    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        let mut children = Vec::<&dyn GenericNode<'_>>::new();
        for param in &self.parameters {
            children.push(param);
        }
        children.push(self.body.as_ref());
        children
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl TokenLiteral for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            join(self.arguments.iter(), ", ")
        )
    }
}

impl<'a> GenericNode<'a> for CallExpression {
    fn name(&self) -> &'static str {
        "Call"
    }

    #[allow(clippy::vec_init_then_push)]
    fn children(&'a self) -> Vec<&'a dyn GenericNode<'a>> {
        let mut children = Vec::<&dyn GenericNode<'_>>::new();
        children.push(self.function.as_ref());
        for arg in &self.arguments {
            children.push(arg);
        }

        children
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::token::TokenKind;
    use expect_test::expect;

    #[test]
    fn display() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    kind: TokenKind::Let,
                    literal: String::from("let"),
                },
                name: Identifier {
                    token: Token {
                        kind: TokenKind::Ident,
                        literal: String::from("myVar"),
                    },
                    value: String::from("myVar"),
                },
                value: Some(Expression::Identifier(Identifier {
                    token: Token {
                        kind: TokenKind::Ident,
                        literal: String::from("anotherVar"),
                    },
                    value: String::from("anotherVar"),
                })),
            })],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn pretty_print() {
        let input = "1 + 2 * 3";

        let mut p = Parser::new(input);
        let program = p.parse_program();

        expect![[r#"
            Program
            └─ Statement "(1 + (2 * 3))"
               └─ Infix "(1 + (2 * 3))"
                  ├─ IntegerLiteral "1"
                  └─ Infix "(2 * 3)"
                     ├─ IntegerLiteral "2"
                     └─ IntegerLiteral "3"
        "#]]
        .assert_eq(&program.pretty_print());
    }
}
