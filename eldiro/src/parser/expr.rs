use crate::lexer::SyntaxKind;

use super::Parser;

enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

enum PrefixOp {
    Neg,
}

impl PrefixOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
        }
    }
}

pub(super) fn expr(p: &mut Parser) {
    expr_binding_power(p, 0);
}

pub(crate) fn expr_binding_power(p: &mut Parser, minimum_binding_power: u8) {
    let checkpoint = p.checkpoint();

    match p.peek() {
        Some(SyntaxKind::Num) | Some(SyntaxKind::Ident) => p.bump(),
        Some(SyntaxKind::Minus) => {
            let op = PrefixOp::Neg;
            let ((), right_binding_power) = op.binding_power();

            p.bump();

            p.start_node_at(checkpoint, SyntaxKind::PrefixExpr);
            expr_binding_power(p, right_binding_power);
            p.finish_node();
        }
        Some(SyntaxKind::ParenL) => {
            p.bump();
            expr_binding_power(p, 0);

            assert_eq!(p.peek(), Some(SyntaxKind::ParenR));
            p.bump();
        }
        _ => {}
    }

    loop {
        let op = match p.peek() {
            Some(SyntaxKind::Plus) => InfixOp::Add,
            Some(SyntaxKind::Minus) => InfixOp::Sub,
            Some(SyntaxKind::Star) => InfixOp::Mul,
            Some(SyntaxKind::Slash) => InfixOp::Div,
            _ => return,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();
        if left_binding_power < minimum_binding_power {
            return;
        }
        p.bump();

        p.start_node_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(p, right_binding_power);
        p.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use super::super::check;
    use expect_test::expect;

    #[test]
    fn parse_number() {
        check(
            "123",
            expect![[r#"
Root@0..3
  Num@0..3 "123""#]],
        )
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
Root@0..7
  Ident@0..7 "counter""#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
Root@0..3
  BinaryExpr@0..3
    Num@0..1 "1"
    Plus@1..2 "+"
    Num@2..3 "2""#]],
        )
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
Root@0..7
  BinaryExpr@0..7
    BinaryExpr@0..5
      BinaryExpr@0..3
        Num@0..1 "1"
        Plus@1..2 "+"
        Num@2..3 "2"
      Plus@3..4 "+"
      Num@4..5 "3"
    Plus@5..6 "+"
    Num@6..7 "4""#]],
        )
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
Root@0..7
  BinaryExpr@0..7
    BinaryExpr@0..5
      Num@0..1 "1"
      Plus@1..2 "+"
      BinaryExpr@2..5
        Num@2..3 "2"
        Star@3..4 "*"
        Num@4..5 "3"
    Minus@5..6 "-"
    Num@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-10",
            expect![[r#"
Root@0..3
  PrefixExpr@0..3
    Minus@0..1 "-"
    Num@1..3 "10""#]],
        )
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20+20",
            expect![[r#"
Root@0..6
  BinaryExpr@0..6
    PrefixExpr@0..3
      Minus@0..1 "-"
      Num@1..3 "20"
    Plus@3..4 "+"
    Num@4..6 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
Root@0..14
  ParenL@0..1 "("
  ParenL@1..2 "("
  ParenL@2..3 "("
  ParenL@3..4 "("
  ParenL@4..5 "("
  ParenL@5..6 "("
  Num@6..8 "10"
  ParenR@8..9 ")"
  ParenR@9..10 ")"
  ParenR@10..11 ")"
  ParenR@11..12 ")"
  ParenR@12..13 ")"
  ParenR@13..14 ")""#]],
        );
    }

    #[test]
    fn parentheses_affect_precedence() {
        check(
            "5*(2+1)",
            expect![[r#"
Root@0..7
  BinaryExpr@0..7
    Num@0..1 "5"
    Star@1..2 "*"
    ParenL@2..3 "("
    BinaryExpr@3..6
      Num@3..4 "2"
      Plus@4..5 "+"
      Num@5..6 "1"
    ParenR@6..7 ")""#]],
        );
    }
}
