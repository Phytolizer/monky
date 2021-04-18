use drop_bomb::DropBomb;

use syntax::SyntaxKind;

use crate::event::Event;
use crate::Parser;

pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("An uncompleted marker was dropped"),
        }
    }

    pub(crate) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
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

pub(crate) struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    pub(crate) fn precede(self, parser: &mut Parser) -> Marker {
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