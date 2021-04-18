use std::mem;

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
        for i in 0..self.events.len() {
            match mem::replace(&mut self.events[i], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut i = i;
                    let mut forward_parent = forward_parent;

                    while let Some(fp) = forward_parent {
                        i += fp;
                        forward_parent = if let Event::StartNode {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[i], Event::Placeholder)
                        {
                            kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!();
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(EldiroLanguage::kind_to_raw(kind));
                    }
                }
                Event::AddToken { kind, text } => self.token(kind, text),
                Event::FinishNode => self.builder.finish_node(),
                Event::Placeholder => {}
            }

            self.eat_trivia();
        }
        self.builder.finish()
    }

    fn token(&mut self, kind: SyntaxKind, text: String) {
        self.builder
            .token(EldiroLanguage::kind_to_raw(kind), text.as_str());
        self.cursor += 1;
    }

    fn eat_trivia(&mut self) {
        while let Some(lexeme) = self.lexemes.get(self.cursor) {
            if !lexeme.kind.is_trivia() {
                break;
            }

            self.token(lexeme.kind, lexeme.text.into());
        }
    }
}
