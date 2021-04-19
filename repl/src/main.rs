use dialoguer::console::style;
use dialoguer::console::Style;
use dialoguer::theme::ColorfulTheme;
use dialoguer::Input;
use log::warn;
use monky::parser::Parser;

fn main() {
    log4rs::init_file("monky-log.yml", Default::default()).unwrap();
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
            warn!("{}", error);
        }

        println!("{}", program);
        println!("{}", program.pretty_print());
    }
}
