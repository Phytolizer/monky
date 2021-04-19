use std::mem;

use lexer::Token;
use rowan::GreenNodeBuilder;
use rowan::Language;
use syntax::EldiroLanguage;
use syntax::SyntaxKind;

use crate::parser::ParseError;
use crate::Parse;

use super::event::Event;

pub(super) struct Sink<'s, 't> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'s>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParseError>,
}

impl<'s, 't> Sink<'s, 't> {
    pub(super) fn new(tokens: &'t [Token<'s>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: vec![],
        }
    }

    pub(super) fn finish(mut self) -> Parse {
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
                Event::Error(error) => self.errors.push(error),
            }

            self.eat_trivia();
        }
        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];
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
