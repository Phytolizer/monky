use logos::Logos;
use std::convert::TryFrom;
use std::ops::Range as StdRange;
use text_size::TextRange;
use text_size::TextSize;

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

        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };

        Some(Self::Item { kind, text, range })
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'s> {
    pub kind: TokenKind,
    pub text: &'s str,
    pub range: TextRange,
}
