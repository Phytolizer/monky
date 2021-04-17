use std::io;

use eldiro::parser::Parser;
use io::Write;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        write!(stdout, "-> ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let parse = Parser::new(&input).parse();
        writeln!(stdout, "{}", parse.debug_tree())?;

        input.clear();
    }
}
