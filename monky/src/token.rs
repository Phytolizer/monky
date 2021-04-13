use std::fmt::Display;

/// The kind of a token.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Star,
    Slash,

    Lt,
    Gt,

    // Delimiters
    Comma,
    Semicolon,

    ParenL,
    ParenR,
    BraceL,
    BraceR,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Illegal => "ILLEGAL",
                TokenKind::Eof => "EOF",
                TokenKind::Ident => "IDENT",
                TokenKind::Int => "INT",
                TokenKind::Assign => "=",
                TokenKind::Plus => "+",
                TokenKind::Comma => ",",
                TokenKind::Semicolon => ";",
                TokenKind::ParenL => "(",
                TokenKind::ParenR => ")",
                TokenKind::BraceL => "{",
                TokenKind::BraceR => "}",
                TokenKind::Function => "FUNCTION",
                TokenKind::Let => "LET",
                TokenKind::Minus => "-",
                TokenKind::Bang => "!",
                TokenKind::Star => "*",
                TokenKind::Slash => "/",
                TokenKind::Lt => "<",
                TokenKind::Gt => ">",
                TokenKind::If => "if",
                TokenKind::Else => "else",
                TokenKind::Return => "return",
                TokenKind::True => "true",
                TokenKind::False => "false",
            }
        )
    }
}

pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}
