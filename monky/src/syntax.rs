use logos::Logos;
use num_derive::FromPrimitive;
use num_derive::ToPrimitive;
use num_traits::FromPrimitive;
use rowan::Language;

#[derive(Debug, PartialEq, Copy, Clone, Logos, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    #[regex(r"[\r\n\t ]+")]
    Whitespace,
    #[error]
    Illegal,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex(r"[1-9][0-9]*|0")]
    Num,

    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Bang,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,

    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,

    #[token("(")]
    ParenL,
    #[token(")")]
    ParenR,
    #[token("{")]
    BraceL,
    #[token("}")]
    BraceR,

    #[token("fn")]
    Function,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Nodes used by the parser.
    Program,
    LetStatement,
    Identifier,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(k: SyntaxKind) -> Self {
        Self(k as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MonkeyLanguage {}

impl Language for MonkeyLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<MonkeyLanguage>;

pub struct Lexer<'s> {
    inner: logos::Lexer<'s, SyntaxKind>,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
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
    use monky_test_macros::test_struct;

    use super::*;

    #[test]
    fn lexer() {
        let input = "
        let five = 5;
        let ten = 10;
        
        let add = fn(x, y) {
            x + y;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        ";
        test_struct! {
            struct {
                kind: SyntaxKind,
                literal: &'static str,
            }{
                {SyntaxKind::Let, "let"},
                {SyntaxKind::Ident, "five"},
                {SyntaxKind::Assign, "="},
                {SyntaxKind::Num, "5"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Let, "let"},
                {SyntaxKind::Ident, "ten"},
                {SyntaxKind::Assign, "="},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Let, "let"},
                {SyntaxKind::Ident, "add"},
                {SyntaxKind::Assign, "="},
                {SyntaxKind::Function, "fn"},
                {SyntaxKind::ParenL, "("},
                {SyntaxKind::Ident, "x"},
                {SyntaxKind::Comma, ","},
                {SyntaxKind::Ident, "y"},
                {SyntaxKind::ParenR, ")"},
                {SyntaxKind::BraceL, "{"},
                {SyntaxKind::Ident, "x"},
                {SyntaxKind::Plus, "+"},
                {SyntaxKind::Ident, "y"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::BraceR, "}"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Let, "let"},
                {SyntaxKind::Ident, "result"},
                {SyntaxKind::Assign, "="},
                {SyntaxKind::Ident, "add"},
                {SyntaxKind::ParenL, "("},
                {SyntaxKind::Ident, "five"},
                {SyntaxKind::Comma, ","},
                {SyntaxKind::Ident, "ten"},
                {SyntaxKind::ParenR, ")"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Bang, "!"},
                {SyntaxKind::Minus, "-"},
                {SyntaxKind::Slash, "/"},
                {SyntaxKind::Star, "*"},
                {SyntaxKind::Num, "5"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Num, "5"},
                {SyntaxKind::Lt, "<"},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::Gt, ">"},
                {SyntaxKind::Num, "5"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::If, "if"},
                {SyntaxKind::ParenL, "("},
                {SyntaxKind::Num, "5"},
                {SyntaxKind::Lt, "<"},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::ParenR, ")"},
                {SyntaxKind::BraceL, "{"},
                {SyntaxKind::Return, "return"},
                {SyntaxKind::True, "true"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::BraceR, "}"},
                {SyntaxKind::Else, "else"},
                {SyntaxKind::BraceL, "{"},
                {SyntaxKind::Return, "return"},
                {SyntaxKind::False, "false"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::BraceR, "}"},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::Eq, "=="},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::Semicolon, ";"},
                {SyntaxKind::Num, "10"},
                {SyntaxKind::Neq, "!="},
                {SyntaxKind::Num, "9"},
                {SyntaxKind::Semicolon, ";"},
            }
        }

        let lexer = Lexer::new(input);
        let lexer_iter = lexer
            .into_iter()
            .filter(|(k, _)| k != &SyntaxKind::Whitespace);
        for ((tk, tt), test) in lexer_iter.zip(tests) {
            assert_eq!(tk, test.kind);
            assert_eq!(tt, test.literal);
        }
    }
}
