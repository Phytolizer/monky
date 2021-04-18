use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::lexer::Lexeme;
use crate::lexer::SyntaxKind;
use crate::syntax::EldiroLanguage;

use super::event::Event;

pub(super) struct Sink<'s, 'l> {
    builder: GreenNodeBuilder<'static>,
    lexemes: &'l [Lexeme<'s>],
    cursor: usize,
    events: Vec<Event>,
}

impl<'s, 'l> Sink<'s, 'l> {
    pub(super) fn new(lexemes: &'l [Lexeme<'s>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            lexemes,
            cursor: 0,
            events,
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        let mut reordered_events = self.events.clone();

        for (i, event) in self.events.iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(i);
                reordered_events.insert(*checkpoint, Event::StartNode { kind: *kind });
            }
        }

        for event in reordered_events {
            match event {
                Event::StartNode { kind } => {
                    self.builder.start_node(EldiroLanguage::kind_to_raw(kind));
                }
                Event::AddToken { kind, text } => self.token(kind, text),
                Event::FinishNode => self.builder.finish_node(),
                Event::StartNodeAt { .. } => unreachable!(),
            }

            self.eat_whitespace();
        }
        self.builder.finish()
    }

    fn token(&mut self, kind: SyntaxKind, text: String) {
        self.builder
            .token(EldiroLanguage::kind_to_raw(kind), text.as_str());
        self.cursor += 1;
    }

    fn eat_whitespace(&mut self) {
        while let Some(lexeme) = self.lexemes.get(self.cursor) {
            if lexeme.kind != SyntaxKind::Whitespace {
                break;
            }

            self.token(lexeme.kind, lexeme.text.into());
        }
    }
}
