#[cfg(test)]
mod tests {
    use monky_test_macros::test_struct;

    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn next_token() {
        let input = "=+(){},;";

        test_struct!(
            struct {
                expected_kind: TokenKind,
                expected_literal: &'static str,
            }{
                {TokenKind::Assign, "="},
                {TokenKind::Plus, "+"},
                {TokenKind::ParenL, "("},
                {TokenKind::ParenR, ")"},
                {TokenKind::BraceL, "{"},
                {TokenKind::BraceR, "}"},
                {TokenKind::Comma, ","},
                {TokenKind::Semicolon, ";"},
            }
        );

        let l = Lexer::new(input);

        for test in tests {
            let tok = l.next_token();

            assert_eq!(tok.kind, test.expected_kind);
            assert_eq!(tok.literal, test.expected_literal);
        }
    }
}
