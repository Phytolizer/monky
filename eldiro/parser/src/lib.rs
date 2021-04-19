use self::sink::Sink;

use crate::parser::Parser;

use crate::parser::ParseError;
#[cfg(test)]
use expect_test::Expect;
use lexer::Lexer;
use rowan::GreenNode;
use source::Source;
use syntax::SyntaxNode;

mod event;
mod grammar;
mod parser;
mod sink;
mod source;

pub fn parse(input: &str) -> Parse {
    let tokens = Lexer::new(input).collect::<Vec<_>>();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<ParseError>,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        s.push_str(&formatted[0..formatted.len() - 1]);

        for error in &self.errors {
            s.push_str(&format!("\n{}", error));
        }

        s
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
