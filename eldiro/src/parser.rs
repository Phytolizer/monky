#[cfg(test)]
use expect_test::Expect;
use rowan::GreenNode;

use crate::lexer::Lexeme;
use crate::lexer::Lexer;
use crate::lexer::SyntaxKind;
use crate::syntax::SyntaxNode;

mod event;
mod expr;
mod sink;

use event::Event;
use expr::expr;
use sink::Sink;

pub fn parse(input: &str) -> Parse {
    let lexemes = Lexer::new(input).collect::<Vec<_>>();
    let parser = Parser::new(&lexemes);
    let events = parser.parse();
    let sink = Sink::new(&lexemes, events);

    Parse {
        green_node: sink.finish(),
    }
}

struct Parser<'s, 'l> {
    lexemes: &'l [Lexeme<'s>],
    cursor: usize,
    events: Vec<Event>,
}

impl<'s, 'l> Parser<'s, 'l> {
    fn new(lexemes: &'l [Lexeme<'s>]) -> Self {
        Self {
            lexemes,
            cursor: 0,
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event> {
        self.start_node(SyntaxKind::Root);

        expr(&mut self);

        self.finish_node();

        self.events
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.eat_whitespace();
        self.peek_raw()
    }

    fn peek_raw(&mut self) -> Option<SyntaxKind> {
        self.lexemes
            .get(self.cursor)
            .map(|Lexeme { kind, .. }| *kind)
    }

    fn eat_whitespace(&mut self) {
        while self.peek_raw() == Some(SyntaxKind::Whitespace) {
            self.cursor += 1;
        }
    }

    fn bump(&mut self) {
        let Lexeme { kind, text } = self.lexemes[self.cursor];
        self.cursor += 1;
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
    let parse = parse(input);
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

    #[test]
    fn parse_whitespace() {
        check(
            "   ",
            expect![[r#"
Root@0..3
  Whitespace@0..3 "   ""#]],
        )
    }
}
