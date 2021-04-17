use logos::Logos;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::lexer::SyntaxKind;
use crate::syntax::EldiroLanguage;
use crate::syntax::SyntaxNode;

pub struct Parser<'s> {
    lexer: logos::Lexer<'s, SyntaxKind>,
    builder: GreenNodeBuilder<'static>,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            lexer: SyntaxKind::lexer(input),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Root);
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
mod tests {
    use expect_test::expect;
    use expect_test::Expect;

    use super::*;

    fn check(input: &str, expected_tree: Expect) {
        let parse = Parser::new(input).parse();
        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]])
    }
}
