use std::iter::Peekable;

use logos::Logos;
use rowan::Checkpoint;
use rowan::GreenNode;
use rowan::GreenNodeBuilder;
use rowan::Language;

use crate::syntax::Lexer;
use crate::syntax::MonkeyLanguage;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxNode;

pub struct Parser<'s> {
    lexer: Peekable<Lexer<'s>>,
    builder: GreenNodeBuilder<'static>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> Parse {
        self.start_node(SyntaxKind::Program);

        while self.peek().is_some() {
            self.statement();
        }

        self.finish_node();

        Parse {
            green_node: self.builder.finish(),
        }
    }

    fn statement(&mut self) {
        match self.peek() {
            Some(SyntaxKind::Let) => self.let_statement(),
            _ => todo!(),
        }
    }

    fn let_statement(&mut self) {
        self.start_node(SyntaxKind::LetStatement);
        self.bump();
        match self.peek() {
            Some(SyntaxKind::Ident) => self.bump(),
            _ => todo!(),
        }
        match self.peek() {
            Some(SyntaxKind::Assign) => self.bump(),
            _ => todo!(),
        }

        loop {
            match self.peek() {
                Some(SyntaxKind::Semicolon) => break,
                Some(_) => {}
                None => todo!(),
            }
            self.bump();
        }

        self.bump();

        self.finish_node();
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(MonkeyLanguage::kind_to_raw(kind));
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(k, _)| *k)
    }

    fn bump(&mut self) {
        let (kind, text) = self.lexer.next().unwrap();
        self.builder
            .token(MonkeyLanguage::kind_to_raw(kind), text.into());
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);
        formatted.trim().to_string()
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::SyntaxNode;

    use super::*;
    use expect_test::expect;
    use expect_test::Expect;

    fn check(input: &str, expected_tree: Expect) {
        let parse = Parser::new(input).parse();
        expected_tree.assert_eq(&parse.debug_tree());
    }

    #[test]
    fn let_statements() {
        check(
            "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ",
            expect![[r#"Program@0..32
  LetStatement@0..7
    Let@0..3 "let"
    Ident@3..4 "x"
    Assign@4..5 "="
    Num@5..6 "5"
    Semicolon@6..7 ";"
  LetStatement@7..15
    Let@7..10 "let"
    Ident@10..11 "y"
    Assign@11..12 "="
    Num@12..14 "10"
    Semicolon@14..15 ";"
  LetStatement@15..32
    Let@15..18 "let"
    Ident@18..24 "foobar"
    Assign@24..25 "="
    Num@25..31 "838383"
    Semicolon@31..32 ";""#]],
        )
    }
}
