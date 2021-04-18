use crate::lexer::SyntaxKind;
use crate::lexer::Token;

pub(super) struct Source<'s, 't> {
    lexemes: &'t [Token<'s>],
    cursor: usize,
}

impl<'s, 't> Source<'s, 't> {
    pub(super) fn new(lexemes: &'t [Token<'s>]) -> Self {
        Self { lexemes, cursor: 0 }
    }

    pub(super) fn next_lexeme(&mut self) -> Option<&'t Token<'s>> {
        self.eat_trivia();

        let lexeme = self.lexemes.get(self.cursor)?;
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
        self.lexemes
            .get(self.cursor)
            .map(|Token { kind, .. }| *kind)
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, SyntaxKind::is_trivia)
    }
}
