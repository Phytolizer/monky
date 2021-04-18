use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

mod expr;
mod stmt;

pub(crate) fn root(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();

    while !parser.at_end() {
        stmt::stmt(parser);
    }

    marker.complete(parser, SyntaxKind::Root)
}
