use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
enum SyntaxKind {
    #[regex(" +")]
    Whitespace,
    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn lex_spaces() {
        let mut lexer = SyntaxKind::lexer("  ");

        assert_eq!(lexer.next(), Some(SyntaxKind::Whitespace));
        assert_eq!(lexer.slice(), "  ");
    }
}
