use std::collections::HashMap;
use std::mem::swap;

use maplit::hashmap;
use once_cell::sync::Lazy;

use crate::ast::Expression;
use crate::ast::ExpressionStatement;
use crate::ast::Identifier;
use crate::ast::InfixExpression;
use crate::ast::IntegerLiteral;
use crate::ast::LetStatement;
use crate::ast::PrefixExpression;
use crate::ast::Program;
use crate::ast::ReturnStatement;
use crate::ast::Statement;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenKind;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
        TokenKind::Bang => parse_prefix_expression as PrefixParseFn,
        TokenKind::Minus => parse_prefix_expression as PrefixParseFn,
    }
});

static INFIX_PARSE_FNS: Lazy<HashMap<TokenKind, InfixParseFn>> = Lazy::new(|| {
    hashmap! {
        TokenKind::Plus => parse_infix_expression as InfixParseFn,
        TokenKind::Minus => parse_infix_expression as InfixParseFn,
        TokenKind::Star => parse_infix_expression as InfixParseFn,
        TokenKind::Slash => parse_infix_expression as InfixParseFn,
        TokenKind::Eq => parse_infix_expression as InfixParseFn,
        TokenKind::Neq => parse_infix_expression as InfixParseFn,
        TokenKind::Lt => parse_infix_expression as InfixParseFn,
        TokenKind::Gt => parse_infix_expression as InfixParseFn,
    }
});

static PRECEDENCES: Lazy<HashMap<TokenKind, Precedence>> = Lazy::new(|| {
    hashmap! {
        TokenKind::Eq => Precedence::Equals,
        TokenKind::Neq => Precedence::Equals,
        TokenKind::Lt => Precedence::LessGreater,
        TokenKind::Gt => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Slash => Precedence::Product,
        TokenKind::Star => Precedence::Product,
    }
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

    fn peek_precedence(&self) -> Precedence {
        *PRECEDENCES
            .get(
                &self
                    .peek_token
                    .as_ref()
                    .map(|t| t.kind)
                    .unwrap_or(TokenKind::Illegal),
            )
            .unwrap_or(&Precedence::Lowest)
    }

    fn cur_precedence(&self) -> Precedence {
        *PRECEDENCES
            .get(&self.cur_token.as_ref().unwrap().kind)
            .unwrap_or(&Precedence::Lowest)
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

    fn no_prefix_parse_fn_error(&mut self, kind: TokenKind) {
        self.errors
            .push(format!("no prefix parse function for {} found", kind));
    }
}

fn parse_expression(p: &mut Parser, precedence: Precedence) -> Option<Expression> {
    let prefix = PREFIX_PARSE_FNS
        .get(&p.cur_token.as_ref().unwrap().kind)
        .or_else(|| {
            p.no_prefix_parse_fn_error(p.cur_token.as_ref().unwrap().kind);
            None
        })?;
    let mut left_exp = prefix(p)?;

    while !p.peek_token_is(TokenKind::Semicolon) && precedence < p.peek_precedence() {
        let infix = match INFIX_PARSE_FNS.get(&p.peek_token.as_ref().unwrap().kind) {
            Some(i) => i,
            None => return Some(left_exp),
        };
        p.next_token();
        left_exp = infix(p, left_exp)?;
    }

    Some(left_exp)
}

#[allow(clippy::unnecessary_wraps)]
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

fn parse_prefix_expression(p: &mut Parser) -> Option<Expression> {
    let token = p.cur_token.as_ref().unwrap().clone();
    let operator = token.literal.clone();

    p.next_token();

    let right = Box::new(parse_expression(p, Precedence::Prefix)?);
    Some(Expression::Prefix(PrefixExpression {
        token,
        operator,
        right,
    }))
}

fn parse_infix_expression(p: &mut Parser, left: Expression) -> Option<Expression> {
    let token = p.cur_token.as_ref().unwrap().clone();
    let operator = token.literal.clone();
    let precedence = p.cur_precedence();
    p.next_token();
    let right = parse_expression(p, precedence)?;
    Some(Expression::Infix(InfixExpression {
        token,
        left: Box::new(left),
        operator,
        right: Box::new(right),
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
    use expect_test::expect;
    use expect_test::Expect;
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

        let tests = vec![expect![["x"]], expect![["y"]], expect![["foobar"]]];

        for (i, expected_identifier) in tests.into_iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, expected_identifier);
        }
    }

    fn check_parser_errors(p: &Parser) {
        expect![[""]].assert_eq(&p.errors().join("\n"));
    }

    fn test_let_statement(stmt: &Statement, expected_identifier: Expect) {
        expect![["let"]].assert_eq(&stmt.token_literal());
        let let_stmt = match stmt {
            Statement::Let(l) => l,
            _ => panic!("expected let statement, got: {:#?}", stmt),
        };
        expected_identifier.assert_eq(&let_stmt.name.value);
        expected_identifier.assert_eq(&let_stmt.name.token_literal());
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

            expect![["return"]].assert_eq(&return_stmt.token_literal());
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
        expect![["foobar"]].assert_eq(&ident.value);
        expect![["foobar"]].assert_eq(&ident.token_literal());
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

    #[test]
    fn parse_prefix_expressions() {
        test_struct! {
            struct {
                input: &'static str,
                operator: &'static str,
                integer_value: i64,
            }{
                {"!5;", "!", 5},
                {"-15;", "-", 15},
            }
        }

        for test in tests {
            let mut p = Parser::new(test.input);
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
            let exp = match &stmt.expression {
                Some(Expression::Prefix(p)) => p,
                _ => panic!(
                    "expected prefix expression to be parsed, got '{}' instead",
                    stmt.expression
                        .as_ref()
                        .map(|x| x.to_string())
                        .unwrap_or_default()
                ),
            };
            assert_eq!(exp.operator, test.operator);
            test_integer_literal(&exp.right, test.integer_value);
        }
    }

    fn test_integer_literal(expr: &Expression, value: i64) {
        let i = match expr {
            Expression::IntegerLiteral(i) => i,
            _ => panic!(
                "expected integer literal to be parsed, got '{}' instead",
                expr
            ),
        };
        assert_eq!(i.value, value);
        assert_eq!(i.token_literal(), value.to_string());
    }

    #[test]
    fn parse_infix_expressions() {
        test_struct! {
            struct {
                input: &'static str,
                left_value: i64,
                operator: &'static str,
                right_value: i64,
            }{
                {"5 + 5;", 5, "+", 5},
                {"5 - 5;", 5, "-", 5},
                {"5 * 5;", 5, "*", 5},
                {"5 / 5;", 5, "/", 5},
                {"5 > 5;", 5, ">", 5},
                {"5 < 5;", 5, "<", 5},
                {"5 == 5;", 5, "==", 5},
                {"5 != 5;", 5, "!=", 5},
            }
        }

        for test in tests {
            let mut p = Parser::new(test.input);
            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.statements.len(), 1);
            let exp = program.statements[0]
                .as_expression()
                .unwrap()
                .expression
                .as_ref()
                .unwrap()
                .as_infix()
                .unwrap();

            test_integer_literal(&exp.left, test.left_value);
            assert_eq!(exp.operator, test.operator);
            test_integer_literal(&exp.right, test.right_value);
        }
    }

    fn check(input: &str, expected: Expect) {
        let mut p = Parser::new(input);
        let program = p.parse_program();
        check_parser_errors(&p);

        expected.assert_eq(&program.to_string());
    }

    #[test]
    fn unary_trumps_infix() {
        check("-a * b", expect![["((-a) * b)"]]);
    }

    #[test]
    fn unary_nests_properly() {
        check("!-a", expect![["(!(-a))"]]);
    }
    #[test]
    fn addition_is_left_associative() {
        check("a + b + c", expect![["((a + b) + c)"]]);
    }
    #[test]
    fn addition_and_subtraction_left_associate() {
        check("a + b - c", expect![["((a + b) - c)"]]);
    }
    #[test]
    fn multiplication_is_left_associative() {
        check("a * b * c", expect![["((a * b) * c)"]]);
    }
    #[test]
    fn multiplication_and_division_left_associate() {
        check("a * b / c", expect![["((a * b) / c)"]]);
    }
    #[test]
    fn division_trumps_addition() {
        check("a + b / c", expect![["(a + (b / c))"]]);
    }
    #[test]
    fn operator_precedence_long_expression() {
        check(
            "a + b * c + d / e - f",
            expect![["(((a + (b * c)) + (d / e)) - f)"]],
        );
    }
    #[test]
    fn operator_precedence_multiple_statements() {
        check("3 + 4; -5 * 5", expect![["(3 + 4)((-5) * 5)"]]);
    }
    #[test]
    fn lt_gt_trump_equality_check() {
        check("5 > 4 == 3 < 4", expect![["((5 > 4) == (3 < 4))"]]);
    }
    #[test]
    fn lt_gt_trump_inequality_check() {
        check("5 < 4 != 3 > 4", expect![["((5 < 4) != (3 > 4))"]]);
    }
    #[test]
    fn arithmetic_trumps_equality() {
        check(
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expect![["((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"]],
        );
    }
}
