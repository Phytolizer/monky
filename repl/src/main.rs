use dialoguer::console::style;
use dialoguer::console::Style;
use dialoguer::theme::ColorfulTheme;
use dialoguer::Input;
use monky::parser::Parser;
use slog::warn;
use slog::Drain;

fn main() {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let log = slog::Logger::root(drain, slog::o!());
    let theme = ColorfulTheme {
        prompt_prefix: style("monky".into()).yellow(),
        prompt_style: Style::new().dim(),
        prompt_suffix: style(">>".into()).bold(),
        success_prefix: style("monky".into()).green(),
        success_suffix: style(">>".into()).bold(),
        values_style: Style::new().blue().bright(),
        ..Default::default()
    };
    let mut input = Input::<String>::with_theme(&theme);
    input.with_prompt(":");

    loop {
        let text = input.interact().unwrap();
        if text == "/q" {
            break;
        }

        let mut parser = Parser::new(&text);
        let program = parser.parse_program();
        for error in parser.errors() {
            warn!(log, "{}", error);
        }

        println!("{}", program);
        println!("{}", program.pretty_print());
    }
}
