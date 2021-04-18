use logos::Logos;

mod token_kind;

pub use self::token_kind::TokenKind;

pub struct Lexer<'s> {
    inner: logos::Lexer<'s, TokenKind>,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some(Self::Item { kind, text })
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'s> {
    pub kind: TokenKind,
    pub text: &'s str,
}
