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
mod source;

use event::Event;
use expr::expr;
use sink::Sink;

use self::source::Source;

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
    source: Source<'s, 'l>,
    events: Vec<Event>,
}

impl<'s, 'l> Parser<'s, 'l> {
    fn new(lexemes: &'l [Lexeme<'s>]) -> Self {
        Self {
            source: Source::new(lexemes),
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
        self.source.peek_kind()
    }

    fn bump(&mut self) {
        let &Lexeme { kind, text } = self.source.next_lexeme().unwrap();
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

    #[test]
    fn parse_comment() {
        check(
            "# test set",
            expect![[r##"
Root@0..10
  Comment@0..10 "# test set""##]],
        )
    }
}
