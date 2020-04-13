use cargo_expand_mods::{Context, Opt};
use std::{
    io::{self, Write as _},
    process,
};
use structopt::StructOpt as _;
use termcolor::{ColorSpec, WriteColor as _};

fn main() {
    let opt = Opt::from_args();

    let (stdout, mut stderr) = (io::stdout(), opt.color().stderr());

    if let Err(err) = Context::with_current_dir(stdout, &mut stderr)
        .and_then(|ctx| cargo_expand_mods::run(opt, ctx))
    {
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
