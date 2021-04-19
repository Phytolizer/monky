use std::mem;

use self::marker::Marker;

use crate::event::Event;
use crate::grammar::root;
use crate::source::Source;
use lexer::Token;

use syntax::SyntaxKind;

pub(crate) mod marker;
mod parse_error;
pub(crate) use parse_error::ParseError;

const RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::KwLet];

pub(crate) struct Parser<'s, 't> {
    source: Source<'s, 't>,
    events: Vec<Event>,
    expected_kinds: Vec<SyntaxKind>,
}

impl<'s, 't> Parser<'s, 't> {
    pub(crate) fn new(source: Source<'s, 't>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        root(&mut self);

        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.expected_kinds.push(kind);
        self.peek() == Some(kind)
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn error(&mut self) {
        let current_token = self.source.peek_token();
        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some((*kind).into()), *range)
        } else {
            (None, self.source.last_token_range().unwrap())
        };
        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        }));
        if !self.at_set(RECOVERY_SET) && !self.at_end() {
            let marker = self.start();
            self.bump();
            marker.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }
}
