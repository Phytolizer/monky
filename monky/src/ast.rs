use crate::token::Token;

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

pub enum Node {
    Expression(Expression),
    Statement(Statement),
    Program(Program),
}

impl TokenLiteral for Node {
    fn token_literal(&self) -> String {
        match self {
            Node::Expression(e) => e.token_literal(),
            Node::Statement(s) => s.token_literal(),
            Node::Program(p) => p.token_literal(),
        }
    }
}

pub enum Expression {}

impl TokenLiteral for Expression {
    fn token_literal(&self) -> String {
        match self {
            _ => unreachable!(),
        }
    }
}

pub enum Statement {
    Let(LetStatement),
}

impl TokenLiteral for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(s) => s.token_literal(),
        }
    }
}

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

#[derive(Default)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    // pub value: Expression,
}

impl TokenLiteral for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Default)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl TokenLiteral for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
