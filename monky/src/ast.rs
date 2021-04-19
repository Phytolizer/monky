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

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Node::Expression(n) => n.to_string(),
                Node::Statement(n) => n.to_string(),
                Node::Program(n) => n.to_string(),
            }
        )
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
}

impl TokenLiteral for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(e) => e.token_literal(),
            Self::IntegerLiteral(i) => i.token_literal(),
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
            }
        )
    }
}

#[derive(Debug)]
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
    pub value: i32,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

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
}
