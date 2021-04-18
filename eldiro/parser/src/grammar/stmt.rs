use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

use super::*;

pub(super) fn stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    match parser.peek() {
        Some(SyntaxKind::KwLet) => variable_def(parser),
        _ => expr::expr(parser),
    }
}

fn variable_def(parser: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(parser.at(SyntaxKind::KwLet));
    let marker = parser.start();
    parser.bump();

    debug_assert!(parser.at(SyntaxKind::Ident));
    parser.bump();

    debug_assert!(parser.at(SyntaxKind::Equals));
    parser.bump();

    expr::expr(parser)?;

    Some(marker.complete(parser, SyntaxKind::VariableDef))
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
}
