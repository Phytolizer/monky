#[cfg(test)]
use expect_test::Expect;
use rowan::GreenNode;

use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxNode;
use lexer::Lexer;
use lexer::Token;

mod event;
mod expr;
mod marker;
mod sink;
mod source;

use event::Event;
use expr::expr;
use sink::Sink;

use self::marker::Marker;
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

struct Parser<'s, 't> {
    source: Source<'s, 't>,
    events: Vec<Event>,
}

impl<'s, 't> Parser<'s, 't> {
    fn new(lexemes: &'t [Token<'s>]) -> Self {
        Self {
            source: Source::new(lexemes),
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event> {
        let m = self.start();
        expr(&mut self);
        m.complete(&mut self, SyntaxKind::Root);

        self.events
    }

    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    fn bump(&mut self) {
        self.source.next_lexeme().unwrap();
        self.events.push(Event::AddToken);
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
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
