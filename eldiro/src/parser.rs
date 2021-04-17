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

mod expr;

use expr::expr;

pub struct Parser<'s> {
    lexer: Peekable<Lexer<'s>>,
    builder: GreenNodeBuilder<'static>,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Root);

        expr(&mut self);

        self.finish_node();

        Parse {
            green_node: self.builder.finish(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(EldiroLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(k, _)| *k)
    }

    fn bump(&mut self) {
        let (kind, text) = self.lexer.next().unwrap();
        self.builder.token(EldiroLanguage::kind_to_raw(kind), text)
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, EldiroLanguage::kind_to_raw(kind));
    }

    fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
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
