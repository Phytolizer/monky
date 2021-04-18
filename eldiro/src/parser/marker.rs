use drop_bomb::DropBomb;

use crate::lexer::SyntaxKind;

use super::event::Event;
use super::Parser;

pub(super) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("An uncompleted marker was dropped"),
        }
    }

    pub(super) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        let event_at_pos = &mut parser.events[self.pos];
        debug_assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };
        parser.events.push(Event::FinishNode);

        self.bomb.defuse();

        CompletedMarker { pos: self.pos }
    }
}

pub(super) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(super) fn precede(self, parser: &mut Parser) -> Marker {
        let new_marker = parser.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = parser.events[self.pos]
        {
            *forward_parent = Some(new_marker.pos - self.pos);
        } else {
            unreachable!();
        }

        new_marker
    }
}
