use rowan::Checkpoint;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::syntax::EldiroLanguage;

use super::event::Event;

pub(super) struct Sink {
    builder: GreenNodeBuilder<'static>,
    events: Vec<Event>,
}

impl Sink {
    pub(super) fn new(events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            events,
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        let mut reordered_events = self.events.clone();

        for (i, event) in self.events.into_iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(i);
                reordered_events.insert(checkpoint, Event::StartNode { kind });
            }
        }

        for event in reordered_events {
            match event {
                Event::StartNode { kind } => {
                    self.builder.start_node(EldiroLanguage::kind_to_raw(kind));
                }
                Event::AddToken { kind, text } => {
                    self.builder.token(EldiroLanguage::kind_to_raw(kind), &text);
                }
                Event::FinishNode => self.builder.finish_node(),
                Event::StartNodeAt { .. } => unreachable!(),
            }
        }
        self.builder.finish()
    }
}
