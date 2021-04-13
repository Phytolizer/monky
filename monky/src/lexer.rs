use std::collections::HashMap;
use std::iter::Peekable;
use std::str::CharIndices;

use maplit::hashmap;
use once_cell::sync::Lazy;

use crate::token::Token;
use crate::token::TokenKind;

use monky_test_macros::lexer_simple_chars;

pub struct Lexer<'s> {
    input: Peekable<CharIndices<'s>>,
    ch: char,
    len: usize,
    position: usize,
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }
}

static KEYWORDS: Lazy<HashMap<&'static str, TokenKind>> = Lazy::new(|| {
    hashmap! {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
    }
});

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        let mut l = Self {
            len: input.len(),
            input: input.char_indices().peekable(),
            ch: '\0',
            position: 0,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        let (np, nc) = self.input.next().unwrap_or((self.len, '\0'));
        self.position = np;
        self.ch = nc;
    }

    fn peek_char(&mut self) -> char {
        let (_, c) = self.input.peek().unwrap_or(&(0, '\0'));
        *c
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while self.ch.is_letter() {
            ident.push(self.ch);
            self.read_char();
        }

        ident
    }

    fn read_number(&mut self) -> String {
        let mut ident = String::new();
        while self.ch.is_ascii_digit() {
            ident.push(self.ch);
            self.read_char();
        }

        ident
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut tok = Token {
            kind: TokenKind::Illegal,
            literal: String::new(),
        };

        self.skip_whitespace();

        if self.ch == '\0' {
            return None;
        }

        lexer_simple_chars! {
            '=' => TokenKind::Assign,
            '+' => TokenKind::Plus,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            '(' => TokenKind::ParenL,
            ')' => TokenKind::ParenR,
            '{' => TokenKind::BraceL,
            '}' => TokenKind::BraceR,
            '-' => TokenKind::Minus,
            '!' => TokenKind::Bang,
            '/' => TokenKind::Slash,
            '*' => TokenKind::Star,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,
        }

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok.kind = TokenKind::Eq;
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    tok.kind = TokenKind::Neq;
                }
            }
            _ => {}
        }

        tok.literal = tok.kind.to_string();

        if tok.kind == TokenKind::Illegal {
            if self.ch.is_letter() {
                tok.literal = self.read_identifier();
                tok.kind = KEYWORDS
                    .get(&tok.literal.as_str())
                    .copied()
                    .unwrap_or(TokenKind::Ident);
                return Some(tok);
            } else if self.ch.is_ascii_digit() {
                tok.literal = self.read_number();
                tok.kind = TokenKind::Int;
                return Some(tok);
            }
        }

        self.read_char();
        Some(tok)
    }
}

#[cfg(test)]
mod tests {
    use monky_test_macros::test_struct;

    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn next_token() {
        let input = "
            let five = 5;
            let ten = 10;
            
            let add = fn(x, y) {
                x + y;
            };
            
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            ";

        test_struct!(
            struct {
                expected_kind: TokenKind,
                expected_literal: &'static str,
            }{
                {TokenKind::Let, "let"},
                {TokenKind::Ident, "five"},
                {TokenKind::Assign, "="},
                {TokenKind::Int, "5"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Let, "let"},
                {TokenKind::Ident, "ten"},
                {TokenKind::Assign, "="},
                {TokenKind::Int, "10"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Let, "let"},
                {TokenKind::Ident, "add"},
                {TokenKind::Assign, "="},
                {TokenKind::Function, "fn"},
                {TokenKind::ParenL, "("},
                {TokenKind::Ident, "x"},
                {TokenKind::Comma, ","},
                {TokenKind::Ident, "y"},
                {TokenKind::ParenR, ")"},
                {TokenKind::BraceL, "{"},
                {TokenKind::Ident, "x"},
                {TokenKind::Plus, "+"},
                {TokenKind::Ident, "y"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::BraceR, "}"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Let, "let"},
                {TokenKind::Ident, "result"},
                {TokenKind::Assign, "="},
                {TokenKind::Ident, "add"},
                {TokenKind::ParenL, "("},
                {TokenKind::Ident, "five"},
                {TokenKind::Comma, ","},
                {TokenKind::Ident, "ten"},
                {TokenKind::ParenR, ")"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Bang, "!"},
                {TokenKind::Minus, "-"},
                {TokenKind::Slash, "/"},
                {TokenKind::Star, "*"},
                {TokenKind::Int, "5"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Int, "5"},
                {TokenKind::Lt, "<"},
                {TokenKind::Int, "10"},
                {TokenKind::Gt, ">"},
                {TokenKind::Int, "5"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::If, "if"},
                {TokenKind::ParenL, "("},
                {TokenKind::Int, "5"},
                {TokenKind::Lt, "<"},
                {TokenKind::Int, "10"},
                {TokenKind::ParenR, ")"},
                {TokenKind::BraceL, "{"},
                {TokenKind::Return, "return"},
                {TokenKind::True, "true"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::BraceR, "}"},
                {TokenKind::Else, "else"},
                {TokenKind::BraceL, "{"},
                {TokenKind::Return, "return"},
                {TokenKind::False, "false"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::BraceR, "}"},
                {TokenKind::Int, "10"},
                {TokenKind::Eq, "=="},
                {TokenKind::Int, "10"},
                {TokenKind::Semicolon, ";"},
                {TokenKind::Int, "10"},
                {TokenKind::Neq, "!="},
                {TokenKind::Int, "9"},
                {TokenKind::Semicolon, ";"},
            }
        );

        let mut l = Lexer::new(input);

        for test in tests {
            let tok = l.next().unwrap();

            eprintln!("{} == {}", tok.kind, test.expected_kind);
            assert_eq!(tok.kind, test.expected_kind);
            eprintln!("'{}' == '{}'", tok.literal, test.expected_literal);
            assert_eq!(tok.literal, test.expected_literal);
        }
        assert!(l.next().is_none());
    }
}
