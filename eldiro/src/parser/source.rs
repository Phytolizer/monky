use crate::syntax::SyntaxKind;

use lexer::Token;

pub(super) struct Source<'s, 't> {
    tokens: &'t [Token<'s>],
    cursor: usize,
}

impl<'s, 't> Source<'s, 't> {
    pub(super) fn new(lexemes: &'t [Token<'s>]) -> Self {
        Self {
            tokens: lexemes,
            cursor: 0,
        }
    }

    pub(super) fn next_lexeme(&mut self) -> Option<&'t Token<'s>> {
        self.eat_trivia();

        let lexeme = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(lexeme)
    }

    pub(super) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn peek_kind_raw(&self) -> Option<SyntaxKind> {
        self.tokens
            .get(self.cursor)
            .map(|Token { kind, .. }| (*kind).into())
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, SyntaxKind::is_trivia)
    }
}
