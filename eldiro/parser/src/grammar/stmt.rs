use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

use super::*;

pub(super) fn stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(SyntaxKind::KwLet) {
        Some(variable_def(parser))
    } else {
        expr::expr(parser)
    }
}

fn variable_def(parser: &mut Parser) -> CompletedMarker {
    debug_assert!(parser.at(SyntaxKind::KwLet));
    let marker = parser.start();
    parser.bump();

    parser.expect(SyntaxKind::Ident);
    parser.expect(SyntaxKind::Equals);

    expr::expr(parser);

    marker.complete(parser, SyntaxKind::VariableDef)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_variable_definition() {
        check(
            "let a = b",
            expect![[r#"
Root@0..9
  VariableDef@0..9
    KwLet@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..5 "a"
    Whitespace@5..6 " "
    Equals@6..7 "="
    Whitespace@7..8 " "
    VariableRef@8..9
      Ident@8..9 "b""#]],
        )
    }

    #[test]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a",
            expect![[r#"
                Root@0..17
                  VariableDef@0..8
                    KwLet@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Equals@6..7 "="
                    Whitespace@7..8 "\n"
                  VariableDef@8..17
                    KwLet@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..13 "b"
                    Whitespace@13..14 " "
                    Equals@14..15 "="
                    Whitespace@15..16 " "
                    VariableRef@16..17
                      Ident@16..17 "a"
                error at 8..11: expected number, identifier, `-` or `(`, but found `let`"#]],
        )
    }

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1\na",
            expect![[r#"
Root@0..11
  VariableDef@0..10
    KwLet@0..3 "let"
    Whitespace@3..4 " "
    Ident@4..5 "a"
    Whitespace@5..6 " "
    Equals@6..7 "="
    Whitespace@7..8 " "
    Literal@8..10
      Num@8..9 "1"
      Whitespace@9..10 "\n"
  VariableRef@10..11
    Ident@10..11 "a""#]],
        )
    }
}
