use std::mem;

use lexer::Token;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::syntax::EldiroLanguage;
use crate::syntax::SyntaxKind;

use super::event::Event;

pub(super) struct Sink<'s, 't> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'s>],
    cursor: usize,
    events: Vec<Event>,
}

impl<'s, 't> Sink<'s, 't> {
    pub(super) fn new(lexemes: &'t [Token<'s>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens: lexemes,
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
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Placeholder => {}
            }

            self.eat_trivia();
        }
        self.builder.finish()
    }

    fn token(&mut self) {
        let Token { kind, text } = self.tokens[self.cursor];
        self.builder
            .token(EldiroLanguage::kind_to_raw(kind.into()), text);
        self.cursor += 1;
    }

    fn eat_trivia(&mut self) {
        while let Some(lexeme) = self.tokens.get(self.cursor) {
            if !SyntaxKind::from(lexeme.kind).is_trivia() {
                break;
            }

            self.token();
        }
    }
}
