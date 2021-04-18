use std::io;

use io::Write;
use eldiro::parser::parse;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        write!(stdout, "-> ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let parse = parse(&input);
        writeln!(stdout, "{}", parse.debug_tree())?;

        input.clear();
    }
}
