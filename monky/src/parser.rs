use std::mem::swap;

use crate::ast::Identifier;
use crate::ast::LetStatement;
use crate::ast::Program;
use crate::ast::Statement;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenKind;

pub struct Parser<'s> {
    lexer: Lexer<'s>,

    cur_token: Option<Token>,
    peek_token: Option<Token>,

    errors: Vec<String>,
}
impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut p = Parser {
            lexer: Lexer::new(source),
            ..Default::default()
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token.is_some() {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.as_ref().unwrap().kind {
            TokenKind::Let => self.parse_let_statement().map(Statement::Let),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let mut stmt = LetStatement {
            token: self.cur_token.as_ref().unwrap().clone(),
            ..Default::default()
        };

        if !self.expect_peek(TokenKind::Ident) {
            return None;
        }

        stmt.name = Identifier {
            token: self.cur_token.as_ref().unwrap().clone(),
            value: self.cur_token.as_ref().unwrap().literal.clone(),
        };

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        while !self.cur_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn cur_token_is(&self, kind: TokenKind) -> bool {
        self.cur_token
            .as_ref()
            .map(|t| t.kind == kind)
            .unwrap_or(false)
    }

    fn peek_token_is(&self, kind: TokenKind) -> bool {
        self.peek_token
            .as_ref()
            .map(|t| t.kind == kind)
            .unwrap_or(false)
    }

    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_token_is(kind) {
            self.next_token();
            true
        } else {
            self.peek_error(kind);
            false
        }
    }

    fn peek_error(&mut self, kind: TokenKind) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            kind,
            self.peek_token.as_ref().unwrap().kind
        ));
    }
}

impl<'s> Default for Parser<'s> {
    fn default() -> Self {
        Self {
            lexer: Lexer::default(),
            cur_token: None,
            peek_token: None,
            errors: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use monky_test_macros::test_struct;

    use crate::ast::Statement;
    use crate::ast::TokenLiteral;

    use super::*;

    #[test]
    fn let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 3358093;
        ";

        let mut p = Parser::new(input);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        test_struct!(
            struct {
                expected_identifier: &'static str,
            }{
                {"x"},
                {"y"},
                {"foobar"},
            }
        );

        for (i, test) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, test.expected_identifier);
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.is_empty() {
            return;
        }

        eprintln!("parser has {} errors", errors.len());
        for msg in errors {
            eprintln!("parser error: {}", msg);
        }
        panic!();
    }

    fn test_let_statement(stmt: &Statement, expected_identifier: &str) {
        assert_eq!(stmt.token_literal(), "let");
        let let_stmt = match stmt {
            Statement::Let(l) => l,
        };
        assert_eq!(let_stmt.name.value, expected_identifier);
        assert_eq!(let_stmt.name.token_literal(), expected_identifier);
    }
}
