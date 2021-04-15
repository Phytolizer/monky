use crate::token::Token;

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expression {}

impl TokenLiteral for Expression {
    fn token_literal(&self) -> String {
        match self {
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl TokenLiteral for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(s) => s.token_literal(),
            Self::Return(s) => s.token_literal(),
        }
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

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct ReturnStatement {
    pub token: Token,
    // pub value: Expression,
}

impl TokenLiteral for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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
