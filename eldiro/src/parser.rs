use std::iter::Peekable;

#[cfg(test)]
use expect_test::Expect;
use rowan::Checkpoint;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::lexer::Lexer;
use crate::lexer::SyntaxKind;
use crate::syntax::EldiroLanguage;
use crate::syntax::SyntaxNode;

mod event;
mod expr;
mod sink;

use event::Event;
use expr::expr;
use sink::Sink;

pub struct Parser<'s> {
    lexer: Peekable<Lexer<'s>>,
    events: Vec<Event>,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            events: vec![],
        }
    }

    pub fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Root);

        expr(&mut self);

        self.finish_node();

        let sink = Sink::new(self.events);

        Parse {
            green_node: sink.finish(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(k, _)| *k)
    }

    fn bump(&mut self) {
        let (kind, text) = self.lexer.next().unwrap();
        self.events.push(Event::AddToken {
            kind,
            text: text.into(),
        });
    }

    fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint });
    }

    fn checkpoint(&self) -> usize {
        self.events.len()
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        formatted[0..formatted.len() - 1].to_string()
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: Expect) {
    let parse = Parser::new(input).parse();
    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]])
    }
}
