use self::marker::Marker;

use crate::event::Event;
use crate::grammar::root;
use crate::source::Source;

use syntax::SyntaxKind;

pub(crate) mod marker;

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
}