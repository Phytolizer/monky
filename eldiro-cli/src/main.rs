use std::io;

use io::Write;
use parser::parse;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        write!(stdout, "-> ")?;
        stdout.flush()?;

        if stdin.read_line(&mut input)? == 0 {
            writeln!(stdout)?;
            break Ok(());
        }

        let parse = parse(&input);
        writeln!(stdout, "{}", parse.debug_tree())?;

        input.clear();
    }
}
