use std::collections::HashMap;
use std::mem::swap;

use maplit::hashmap;
use once_cell::sync::Lazy;

use crate::ast::Expression;
use crate::ast::ExpressionStatement;
use crate::ast::Identifier;
use crate::ast::IntegerLiteral;
use crate::ast::LetStatement;
use crate::ast::Program;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenKind;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

static PREFIX_PARSE_FNS: Lazy<HashMap<TokenKind, PrefixParseFn>> = Lazy::new(|| {
    hashmap! {
        TokenKind::Ident => parse_identifier as PrefixParseFn,
        TokenKind::Int => parse_integer_literal as PrefixParseFn,
    }
});

static INFIX_PARSE_FNS: Lazy<HashMap<TokenKind, InfixParseFn>> = Lazy::new(|| {
    hashmap! {}
});

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
            TokenKind::Return => self.parse_return_statement().map(Statement::Return),
            _ => self.parse_expression_statement().map(Statement::Expression),
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

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        let mut stmt = ReturnStatement {
            token: self.cur_token.as_ref().unwrap().clone(),
            ..Default::default()
        };

        self.next_token();

        while !self.cur_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let mut stmt = ExpressionStatement {
            token: self.cur_token.as_ref().unwrap().clone(),
            ..Default::default()
        };
        stmt.expression = parse_expression(self, Precedence::Lowest);

        if self.peek_token_is(TokenKind::Semicolon) {
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

fn parse_expression(p: &mut Parser, precedence: Precedence) -> Option<Expression> {
    let prefix = PREFIX_PARSE_FNS.get(&p.cur_token.as_ref().unwrap().kind)?;
    let left_exp = prefix(p);

    left_exp
}

fn parse_identifier(p: &mut Parser) -> Option<Expression> {
    Some(Expression::Identifier(Identifier {
        token: p.cur_token.as_ref().unwrap().clone(),
        value: p.cur_token.as_ref().unwrap().literal.clone(),
    }))
}

fn parse_integer_literal(p: &mut Parser) -> Option<Expression> {
    Some(Expression::IntegerLiteral(IntegerLiteral {
        token: p.cur_token.as_ref().unwrap().clone(),
        value: p
            .cur_token
            .as_ref()
            .unwrap()
            .literal
            .parse()
            .ok()
            .or_else(|| {
                p.errors.push(format!(
                    "could not parse '{}' as integer",
                    p.cur_token.as_ref().unwrap().literal
                ));
                None
            })?,
    }))
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
            _ => panic!("expected let statement, got: {:#?}", stmt),
        };
        assert_eq!(let_stmt.name.value, expected_identifier);
        assert_eq!(let_stmt.name.token_literal(), expected_identifier);
    }

    #[test]
    fn return_statements() {
        let input = "
        return 5;
        return 10;
        return 943290;
        ";
        let mut p = Parser::new(input);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 3);
        for stmt in program.statements {
            let return_stmt = match stmt {
                Statement::Return(r) => r,
                _ => panic!("expected return statement, got: {:#?}", stmt),
            };

            assert_eq!(return_stmt.token_literal(), "return");
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let mut p = Parser::new(input);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let expression_statement = match &program.statements[0] {
            Statement::Expression(e) => e,
            _ => panic!(
                "expected expression statement to be parsed, got '{}' instead",
                program.statements[0]
            ),
        };
        let ident = match &expression_statement.expression {
            Some(Expression::Identifier(i)) => i,
            _ => panic!(
                "expected identifier to be parsed, got '{}' instead",
                expression_statement
                    .expression
                    .as_ref()
                    .map(|x| x.to_string())
                    .unwrap_or_default()
            ),
        };
        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token_literal(), "foobar");
    }

    #[test]
    fn integer_literal_expression() {
        let input = "4;";

        let mut p = Parser::new(input);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.statements.len(), 1);
        let stmt = match &program.statements[0] {
            Statement::Expression(e) => e,
            _ => panic!(
                "expected expression statement to be parsed, got '{}' instead",
                program.statements[0]
            ),
        };
        let literal = match &stmt.expression {
            Some(Expression::IntegerLiteral(i)) => i,
            _ => panic!(
                "expected integer literal to be parsed, got '{}' instead",
                stmt.expression
                    .as_ref()
                    .map(|x| x.to_string())
                    .unwrap_or_default()
            ),
        };
        assert_eq!(literal.value, 4);
        assert_eq!(literal.token_literal(), "4");
    }
}
