use cargo_expand_mods::Opt;
use structopt::StructOpt as _;

fn main() -> anyhow::Result<()> {
    cargo_expand_mods::run(Opt::from_args())
}
