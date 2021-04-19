use lexer::Token;
use syntax::SyntaxKind;
use text_size::TextRange;

pub(super) struct Source<'s, 't> {
    tokens: &'t [Token<'s>],
    cursor: usize,
}

impl<'s, 't> Source<'s, 't> {
    pub(super) fn new(tokens: &'t [Token<'s>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    pub(crate) fn next_token(&mut self) -> Option<&Token> {
        self.eat_trivia();
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    fn peek_token_raw(&mut self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    pub(crate) fn peek_kind(&mut self) -> Option<SyntaxKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    fn peek_kind_raw(&mut self) -> Option<SyntaxKind> {
        self.peek_token_raw()
            .map(|Token { kind, .. }| (*kind).into())
    }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&mut self) -> bool {
        self.peek_kind_raw().map_or(false, SyntaxKind::is_trivia)
    }
}
