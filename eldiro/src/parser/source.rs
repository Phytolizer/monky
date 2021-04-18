use crate::lexer::Lexeme;
use crate::lexer::SyntaxKind;

pub(super) struct Source<'s, 'l> {
    lexemes: &'l [Lexeme<'s>],
    cursor: usize,
}

impl<'s, 'l> Source<'s, 'l> {
    pub(super) fn new(lexemes: &'l [Lexeme<'s>]) -> Self {
        Self { lexemes, cursor: 0 }
    }

    pub(super) fn next_lexeme(&mut self) -> Option<&'l Lexeme<'s>> {
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
            .map(|Lexeme { kind, .. }| *kind)
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, SyntaxKind::is_trivia)
    }
}
