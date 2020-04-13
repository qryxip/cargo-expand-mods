use cargo_expand_mods::Opt;
use std::{io::Write as _, process};
use structopt::StructOpt as _;
use termcolor::{BufferedStandardStream, ColorChoice, ColorSpec, WriteColor as _};

fn main() {
    let opt = Opt::from_args();
    let mut stderr = BufferedStandardStream::stderr(ColorChoice::Auto);

    if let Err(err) = cargo_expand_mods::run(opt) {
        let _ = stderr.set_color(
            ColorSpec::new()
                .set_fg(Some(termcolor::Color::Red))
                .set_bold(true)
                .set_reset(false),
        );
        let _ = stderr.write_all(b"error: ");
        let _ = stderr.reset();
        let _ = writeln!(stderr, "{}", err);

        for err in err.chain().skip(1) {
            let _ = writeln!(stderr, "\nCaused by:\n  {}", err);
        }

        let _ = stderr.flush();
        process::exit(101);
    }
}
