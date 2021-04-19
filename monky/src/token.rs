use std::fmt::Display;

/// The kind of a token.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
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
    Eq,
    Neq,

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
                TokenKind::Eof => "",
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
                TokenKind::Function => "fn",
                TokenKind::Let => "let",
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
                TokenKind::Eq => "==",
                TokenKind::Neq => "!=",
            }
        )
    }
}

impl Default for TokenKind {
    fn default() -> Self {
        Self::Illegal
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::default(),
            literal: String::default(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}
