use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

enum UnaryOp {
    Neg,
}

impl UnaryOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
        }
    }
}

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(p, 0)
}

fn lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    let cm = match parser.peek() {
        Some(SyntaxKind::Num) => literal(parser),
        Some(SyntaxKind::Ident) => variable_ref(parser),
        Some(SyntaxKind::Minus) => prefix_expr(parser),
        Some(SyntaxKind::ParenL) => paren_expr(parser),
        _ => return None, // TODO handle error
    };
    Some(cm)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(SyntaxKind::ParenL));

    let m = parser.start();
    parser.bump();
    expr_binding_power(parser, 0);
    debug_assert_eq!(parser.peek(), Some(SyntaxKind::ParenR));

    debug_assert!(parser.at(SyntaxKind::ParenR));
    parser.bump();
    m.complete(parser, SyntaxKind::ParenExpr)
}

fn prefix_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::Minus));

    let m = parser.start();
    let op = UnaryOp::Neg;
    let ((), right_binding_power) = op.binding_power();
    parser.bump();
    expr_binding_power(parser, right_binding_power);
    m.complete(parser, SyntaxKind::PrefixExpr)
}

fn variable_ref(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(SyntaxKind::Ident));

    let m = parser.start();
    parser.bump();
    m.complete(parser, SyntaxKind::VariableRef)
}

fn literal(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(SyntaxKind::Num));

    let m = parser.start();
    parser.bump();
    m.complete(parser, SyntaxKind::Literal)
}

pub(crate) fn expr_binding_power(
    parser: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    let mut lhs = lhs(parser)?;

    loop {
        let op = match parser.peek() {
            Some(SyntaxKind::Plus) => BinaryOp::Add,
            Some(SyntaxKind::Minus) => BinaryOp::Sub,
            Some(SyntaxKind::Star) => BinaryOp::Mul,
            Some(SyntaxKind::Slash) => BinaryOp::Div,
            _ => break,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();
        if left_binding_power < minimum_binding_power {
            return Some(lhs);
        }
        parser.bump();

        let m = lhs.precede(parser);
        expr_binding_power(parser, right_binding_power);
        lhs = m.complete(parser, SyntaxKind::InfixExpr);
    }

    Some(lhs)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_number() {
        check(
            "123",
            expect![[r#"
                Root@0..3
                  Literal@0..3
                    Num@0..3 "123""#]],
        )
    }

    #[test]
    fn parse_number_preceded_by_whitespace() {
        check(
            "   9876",
            expect![[r#"
                Root@0..7
                  Whitespace@0..3 "   "
                  Literal@3..7
                    Num@3..7 "9876""#]],
        )
    }

    #[test]
    fn parse_number_followed_by_whitespace() {
        check(
            "1234  ",
            expect![[r#"
                Root@0..6
                  Literal@0..6
                    Num@0..4 "1234"
                    Whitespace@4..6 "  ""#]],
        )
    }

    #[test]
    fn parse_number_surrounded_by_whitespace() {
        check(
            "  12 ",
            expect![[r#"
                Root@0..5
                  Whitespace@0..2 "  "
                  Literal@2..5
                    Num@2..4 "12"
                    Whitespace@4..5 " ""#]],
        )
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                Root@0..7
                  VariableRef@0..7
                    Ident@0..7 "counter""#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
                Root@0..3
                  InfixExpr@0..3
                    Literal@0..1
                      Num@0..1 "1"
                    Plus@1..2 "+"
                    Literal@2..3
                      Num@2..3 "2""#]],
        )
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
                Root@0..7
                  InfixExpr@0..7
                    InfixExpr@0..5
                      InfixExpr@0..3
                        Literal@0..1
                          Num@0..1 "1"
                        Plus@1..2 "+"
                        Literal@2..3
                          Num@2..3 "2"
                      Plus@3..4 "+"
                      Literal@4..5
                        Num@4..5 "3"
                    Plus@5..6 "+"
                    Literal@6..7
                      Num@6..7 "4""#]],
        )
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
                Root@0..7
                  InfixExpr@0..7
                    InfixExpr@0..5
                      Literal@0..1
                        Num@0..1 "1"
                      Plus@1..2 "+"
                      InfixExpr@2..5
                        Literal@2..3
                          Num@2..3 "2"
                        Star@3..4 "*"
                        Literal@4..5
                          Num@4..5 "3"
                    Minus@5..6 "-"
                    Literal@6..7
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
                    Literal@1..3
                      Num@1..3 "10""#]],
        )
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20+20",
            expect![[r#"
                Root@0..6
                  InfixExpr@0..6
                    PrefixExpr@0..3
                      Minus@0..1 "-"
                      Literal@1..3
                        Num@1..3 "20"
                    Plus@3..4 "+"
                    Literal@4..6
                      Num@4..6 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
                Root@0..14
                  ParenExpr@0..14
                    ParenL@0..1 "("
                    ParenExpr@1..13
                      ParenL@1..2 "("
                      ParenExpr@2..12
                        ParenL@2..3 "("
                        ParenExpr@3..11
                          ParenL@3..4 "("
                          ParenExpr@4..10
                            ParenL@4..5 "("
                            ParenExpr@5..9
                              ParenL@5..6 "("
                              Literal@6..8
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
                  InfixExpr@0..7
                    Literal@0..1
                      Num@0..1 "5"
                    Star@1..2 "*"
                    ParenExpr@2..7
                      ParenL@2..3 "("
                      InfixExpr@3..6
                        Literal@3..4
                          Num@3..4 "2"
                        Plus@4..5 "+"
                        Literal@5..6
                          Num@5..6 "1"
                      ParenR@6..7 ")""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                Root@0..12
                  Whitespace@0..1 " "
                  InfixExpr@1..12
                    Literal@1..3
                      Num@1..2 "1"
                      Whitespace@2..3 " "
                    Plus@3..4 "+"
                    Whitespace@4..7 "   "
                    InfixExpr@7..12
                      Literal@7..8
                        Num@7..8 "2"
                      Star@8..9 "*"
                      Whitespace@9..10 " "
                      Literal@10..12
                        Num@10..11 "3"
                        Whitespace@11..12 " ""#]],
        );
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 # Add one
  + 10 # Add ten",
            expect![[r##"
                Root@0..35
                  Whitespace@0..1 "\n"
                  InfixExpr@1..35
                    InfixExpr@1..21
                      Literal@1..5
                        Num@1..2 "1"
                        Whitespace@2..5 "\n  "
                      Plus@5..6 "+"
                      Whitespace@6..7 " "
                      Literal@7..21
                        Num@7..8 "1"
                        Whitespace@8..9 " "
                        Comment@9..18 "# Add one"
                        Whitespace@18..21 "\n  "
                    Plus@21..22 "+"
                    Whitespace@22..23 " "
                    Literal@23..35
                      Num@23..25 "10"
                      Whitespace@25..26 " "
                      Comment@26..35 "# Add ten""##]],
        );
    }
}