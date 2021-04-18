use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
pub enum TokenKind {
    #[regex("#.*")]
    Comment,
    #[regex("[ \n]+")]
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
    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;
    use crate::Token;

    fn check_single(text: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(text);
        assert_eq!(lexer.next(), Some(Token { kind, text }));
    }

    #[test]
    fn lex_spaces() {
        check_single("  ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_comment() {
        check_single("# test", TokenKind::Comment);
    }

    #[test]
    fn lex_spaces_and_newlines() {
        check_single("  \n ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_fn_keyword() {
        check_single("fn", TokenKind::KwFn);
    }

    #[test]
    fn lex_let_keyword() {
        check_single("let", TokenKind::KwLet);
    }

    #[test]
    fn lex_single_char_identifier() {
        check_single("x", TokenKind::Ident);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check_single("abcdefg", TokenKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check_single("abc123", TokenKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check_single("abABcD3", TokenKind::Ident);
    }

    #[test]
    fn lex_number() {
        check_single("489320", TokenKind::Num);
    }

    #[test]
    fn lex_plus() {
        check_single("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check_single("-", TokenKind::Minus);
    }

    #[test]
    fn lex_star() {
        check_single("*", TokenKind::Star);
    }

    #[test]
    fn lex_slash() {
        check_single("/", TokenKind::Slash);
    }

    #[test]
    fn lex_equals() {
        check_single("=", TokenKind::Equals);
    }

    #[test]
    fn lex_left_brace() {
        check_single("{", TokenKind::BraceL);
    }

    #[test]
    fn lex_right_brace() {
        check_single("}", TokenKind::BraceR);
    }

    #[test]
    fn lex_left_parenthesis() {
        check_single("(", TokenKind::ParenL);
    }

    #[test]
    fn lex_right_parenthesis() {
        check_single(")", TokenKind::ParenR);
    }
}
