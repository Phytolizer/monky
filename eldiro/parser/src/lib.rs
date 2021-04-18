use self::expr::expr;
use self::sink::Sink;

use crate::parser::Parser;

#[cfg(test)]
use expect_test::Expect;
use lexer::Lexer;
use rowan::GreenNode;
use source::Source;
use syntax::SyntaxNode;

mod event;
mod expr;
mod sink;
mod source;

mod parser;

pub fn parse(input: &str) -> Parse {
    let tokens = Lexer::new(input).collect::<Vec<_>>();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    Parse {
        green_node: sink.finish(),
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
