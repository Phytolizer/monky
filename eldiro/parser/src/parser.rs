use self::marker::Marker;

use crate::event::Event;
use crate::grammar::root;
use crate::source::Source;

use syntax::SyntaxKind;

pub(crate) mod marker;

const RECOVERY_SET: &[SyntaxKind] = &[SyntaxKind::KwLet];

pub(crate) struct Parser<'s, 't> {
    source: Source<'s, 't>,
    events: Vec<Event>,
}

impl<'s, 't> Parser<'s, 't> {
    pub(crate) fn new(source: Source<'s, 't>) -> Self {
        Self {
            source,
            events: vec![],
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

    pub(crate) fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn bump(&mut self) {
        self.source.next_lexeme().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
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
        if !self.at_set(RECOVERY_SET) && !self.at_end() {
            self.bump();
        }
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn at_set(&mut self, set: &[SyntaxKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }
}
