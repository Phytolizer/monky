use logos::Logos;
use num_derive::FromPrimitive;
use num_derive::ToPrimitive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos, FromPrimitive, ToPrimitive)]
pub(crate) enum SyntaxKind {
    #[regex(" +")]
    Whitespace,
    #[error]
    Error,

    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex(r"[0-9]+")]
    Num,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equals,
    #[token("{")]
    BraceL,
    #[token("}")]
    BraceR,

    // AST nodes.
    Root,
}

pub(crate) struct Lexer<'s> {
    inner: logos::Lexer<'s, SyntaxKind>,
}

impl<'s> Lexer<'s> {
    pub(crate) fn new(input: &'s str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = (SyntaxKind, &'s str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some((kind, text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_single(input: &str, expected_kind: SyntaxKind) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next(), Some((expected_kind, input)));
    }

    #[test]
    fn lex_spaces() {
        check_single("  ", SyntaxKind::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check_single("fn", SyntaxKind::KwFn);
    }

    #[test]
    fn lex_let_keyword() {
        check_single("let", SyntaxKind::KwLet);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check_single("abcdefg", SyntaxKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check_single("abc123", SyntaxKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check_single("abABcD3", SyntaxKind::Ident);
    }

    #[test]
    fn lex_number() {
        check_single("489320", SyntaxKind::Num);
    }

    #[test]
    fn lex_plus() {
        check_single("+", SyntaxKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check_single("-", SyntaxKind::Minus);
    }

    #[test]
    fn lex_star() {
        check_single("*", SyntaxKind::Star);
    }

    #[test]
    fn lex_slash() {
        check_single("/", SyntaxKind::Slash);
    }

    #[test]
    fn lex_equals() {
        check_single("=", SyntaxKind::Equals);
    }

    #[test]
    fn lex_left_brace() {
        check_single("{", SyntaxKind::BraceL);
    }

    #[test]
    fn lex_right_brace() {
        check_single("}", SyntaxKind::BraceR);
    }
}
