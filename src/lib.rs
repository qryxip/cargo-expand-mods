//! All items are `hidden`.

use anyhow::{anyhow, bail, Context as _};
use arrayvec::ArrayVec;
use cargo_metadata::{CargoOpt, MetadataCommand, Package, Resolve, Target};
use derivative::Derivative;
use duct::cmd;
use fixedbitset::FixedBitSet;
use itertools::Itertools as _;
use proc_macro2::LineColumn;
use serde::Deserialize;
use smallvec::SmallVec;
use std::{
    fmt::Display,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
    {env, fs, iter, str},
};
use structopt::{clap::AppSettings, StructOpt};
use strum::{EnumString, EnumVariantNames, VariantNames as _};
use syn::{spanned::Spanned as _, Item, Lit, Meta, MetaNameValue};
use termcolor::{BufferedStandardStream, Color, ColorSpec, WriteColor};
use url::Url;

#[doc(hidden)]
#[derive(StructOpt, Debug)]
#[structopt(
    author,
    about,
    bin_name("cargo"),
    global_settings(&[AppSettings::DeriveDisplayOrder, AppSettings::UnifiedHelpMessage])
)]
pub enum Opt {
    ExpandMods {
        /// [cargo] Space-separated list of features to activate
        #[structopt(long, value_name("FEATURES"), min_values(1))]
        features: Vec<String>,

        /// [cargo] Activate all available features
        #[structopt(long)]
        all_features: bool,

        /// [cargo] Do not activate the `default` feature
        #[structopt(long)]
        no_default_features: bool,

        /// [cargo] Path to Cargo.toml
        #[structopt(long, value_name("PATH"))]
        manifest_path: Option<PathBuf>,

        /// [cargo] Coloring
        #[structopt(
            long,
            value_name("WHEN"),
            case_insensitive(false),
            default_value("auto"),
            possible_values(ColorChoice::VARIANTS)
        )]
        color: ColorChoice,

        /// [cargo] Require Cargo.lock and cache are up to date
        #[structopt(long)]
        frozen: bool,

        /// [cargo] Require Cargo.lock is up to date
        #[structopt(long)]
        locked: bool,

        /// [cargo] Run without accessing the network
        #[structopt(long)]
        offline: bool,

        /// Do not attempt to run rustfmt
        #[structopt(long)]
        ugly: bool,

        /// Package with the target to expand
        #[structopt(short, long, value_name("SPEC"))]
        package: Option<String>,

        /// Expand this package's library
        #[structopt(long)]
        lib: bool,

        /// Expand the specified binary
        #[structopt(long, value_name("NAME"))]
        bin: Option<String>,

        /// Expand the specified example
        #[structopt(long, value_name("NAME"))]
        example: Option<String>,

        /// Expand the specified test target
        #[structopt(long, value_name("NAME"))]
        test: Option<String>,

        /// Expand the specified bench target
        #[structopt(long, value_name("NAME"))]
        bench: Option<String>,
    },
}

impl Opt {
    pub fn color(&self) -> ColorChoice {
        let Self::ExpandMods { color, .. } = *self;
        color
    }
}

#[doc(hidden)]
pub struct Context<O, E> {
    pub cwd: PathBuf,
    pub stdout: O,
    pub stderr: E,
}

impl<O: Write, E: WriteColor> Context<O, E> {
    pub fn with_current_dir(stdout: O, stderr: E) -> anyhow::Result<Self> {
        let cwd = env::current_dir().with_context(|| "failed to get CWD")?;
        Ok(Self {
            cwd,
            stdout,
            stderr,
        })
    }
}

#[doc(hidden)]
#[derive(EnumString, EnumVariantNames, Debug, Clone, Copy)]
#[strum(serialize_all = "kebab-case")]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

impl ColorChoice {
    pub fn stderr(self) -> BufferedStandardStream {
        BufferedStandardStream::stderr(match self {
            crate::ColorChoice::Auto if atty::is(atty::Stream::Stderr) => {
                termcolor::ColorChoice::Auto
            }
            crate::ColorChoice::Always => termcolor::ColorChoice::Always,
            _ => termcolor::ColorChoice::Never,
        })
    }
}

#[doc(hidden)]
pub fn run<O: Write, E: WriteColor>(opt: Opt, ctx: Context<O, E>) -> anyhow::Result<()> {
    let Opt::ExpandMods {
        features,
        all_features,
        no_default_features,
        manifest_path,
        frozen,
        locked,
        offline,
        ugly,
        package,
        lib,
        bin,
        example,
        test,
        bench,
        ..
    } = opt;

    let Context {
        cwd,
        mut stdout,
        mut stderr,
    } = ctx;

    let metadata = {
        let mut cmd = MetadataCommand::new();
        if !features.is_empty() {
            cmd.features(CargoOpt::SomeFeatures(features));
        }
        if all_features {
            cmd.features(CargoOpt::AllFeatures);
        }
        if no_default_features {
            cmd.features(CargoOpt::NoDefaultFeatures);
        }
        if let Some(manifest_path) = &manifest_path {
            cmd.manifest_path(manifest_path);
        }
        if frozen {
            cmd.other_options(&["--frozen".to_owned()]);
        }
        if locked {
            cmd.other_options(&["--locked".to_owned()]);
        }
        if offline {
            cmd.other_options(&["--offline".to_owned()]);
        }
        cmd.current_dir(cwd).exec()?
    };

    let mut members = metadata
        .packages
        .iter()
        .filter(|Package { id, .. }| metadata.workspace_members.contains(id));

    let packages = if let Some(package) = package {
        let cargo_exe = env::var_os("CARGO").with_context(|| "`$CARGO` should be present")?;

        let manifest_path = metadata
            .resolve
            .as_ref()
            .and_then(|Resolve { root, .. }| root.as_ref())
            .map(|id| metadata[id].manifest_path.clone())
            .unwrap_or_else(|| metadata.workspace_root.join("Cargo.toml"));

        let output = Command::new(&cargo_exe)
            .arg("pkgid")
            .arg("--manifest-path")
            .arg(manifest_path)
            .arg(&package)
            .output()?;

        let stdout = str::from_utf8(&output.stdout)?.trim_end();
        let stderr = str::from_utf8(&output.stderr)?.trim_end();

        if !output.status.success() {
            bail!("{}", stderr.trim_start_matches("error: "));
        }

        let url = stdout.parse::<Url>()?;
        let fragment = url.fragment().expect("the URL should contain fragment");
        let spec_name = match *fragment.splitn(2, ':').collect::<Vec<_>>() {
            [name, _] => name,
            [_] => url
                .path_segments()
                .and_then(Iterator::last)
                .expect("should contain name"),
            _ => unreachable!(),
        };

        let package = members
            .find(|Package { name, .. }| name == spec_name)
            .with_context(|| format!("package `{}` is not a member of the workspace", package))?;
        vec![package]
    } else {
        members.collect()
    };

    let default_run = if let Ok(package) = packages.iter().exactly_one() {
        fs::read_to_string(&package.manifest_path)
            .map_err(anyhow::Error::from)
            .and_then(|cargo_toml| toml::from_str::<CargoToml>(&cargo_toml).map_err(Into::into))
            .with_context(|| format!("Could not read {}", package.manifest_path.to_str().unwrap()))?
            .package
            .and_then(|CargoTomlPackage { default_run, .. }| default_run)
    } else {
        None
    };

    let mut targets = packages
        .into_iter()
        .flat_map(|p| p.targets.iter().map(move |t| (t, p)));

    let (Target { name, src_path, .. }, Package { manifest_path, .. }) = if lib {
        targets
            .filter(|(t, _)| {
                t.kind.contains(&"lib".to_owned()) || t.kind.contains(&"proc-macro".to_owned())
            })
            .exactly_one()
            .map_err(
                |err| match &*err.map(|(_, p)| &p.name).collect::<Vec<_>>() {
                    [] => anyhow!("no lib target"),
                    names => anyhow!("multiple lib targets in the workspace: {:?}", names),
                },
            )
    } else if let Some(bin) = bin.or(default_run) {
        targets
            .find(|(t, _)| t.name == bin && t.kind.contains(&"bin".to_owned()))
            .with_context(|| format!("no bin target named `{}`", bin))
    } else if let Some(example) = example {
        targets
            .find(|(t, _)| t.name == example && t.kind.contains(&"example".to_owned()))
            .with_context(|| format!("no example target named `{}`", example))
    } else if let Some(test) = test {
        targets
            .find(|(t, _)| t.name == test && t.kind.contains(&"test".to_owned()))
            .with_context(|| format!("no test target named `{}`", test))
    } else if let Some(bench) = bench {
        targets
            .find(|(t, _)| t.name == bench && t.kind.contains(&"bench".to_owned()))
            .with_context(|| format!("no bench target named `{}`", bench))
    } else {
        Err(anyhow!(
            "could not determine which target to expand. Use `--lib`, `--bin`, `--example`, `--test`, `--bench`, or `--package` option to specify a target",
        ))
    }?;

    stderr.status_with_color("Expanding", format_args!("`{}`", name), Color::Green)?;

    let mut expanded = expand_mods(&src_path)?;

    if !ugly {
        let rustfmt_exe = (|| {
            let stdout = cmd!(env::var_os("CARGO")?, "--version").read().ok()?;
            let probably_channel = stdout
                .trim_start_matches("cargo ")
                .splitn(2, ' ')
                .next()
                .unwrap();
            cmd!(
                which::which("rustup").ok()?,
                "which",
                "--toolchain",
                probably_channel,
                "rustfmt",
            )
            .read()
            .ok()
        })()
        .unwrap_or_else(|| "rustfmt".to_owned());

        let cargo_toml = fs::read_to_string(manifest_path)
            .with_context(|| format!("failed to read `{}`", manifest_path.display()))?;
        let cargo_toml = toml::from_str::<CargoToml>(&cargo_toml).with_context(|| {
            format!(
                "failed to parse the manifest at `{}`",
                manifest_path.display(),
            )
        })?;
        let edition = cargo_toml
            .package
            .map(|CargoTomlPackage { edition, .. }| edition)
            .unwrap_or_default();

        stderr.status_with_color(
            "Formatting",
            format_args!("with `{}`", rustfmt_exe),
            Color::Green,
        )?;

        let output = cmd!(rustfmt_exe, "--edition", <&str>::from(edition))
            .stdin_bytes(expanded)
            .stdout_capture()
            .dir(manifest_path.parent().expect("should not be empty"))
            .run()?;

        expanded = String::from_utf8(output.stdout)?;
    }

    stdout.write_all(expanded.as_ref())?;
    stdout.flush().map_err(Into::into)
}

#[derive(Deserialize, Debug)]
struct CargoToml {
    #[serde(default)]
    package: Option<CargoTomlPackage>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
struct CargoTomlPackage {
    edition: Edition,
    #[serde(default)]
    default_run: Option<String>,
}

#[derive(Deserialize, Debug, Derivative)]
#[derivative(Default)]
enum Edition {
    #[serde(rename = "2015")]
    #[derivative(Default)]
    Edition2015,
    #[serde(rename = "2018")]
    Edition2018,
}

impl From<Edition> for &'static str {
    fn from(edition: Edition) -> &'static str {
        match edition {
            Edition::Edition2015 => "2015",
            Edition::Edition2018 => "2018",
        }
    }
}

fn expand_mods(src_path: &Path) -> anyhow::Result<String> {
    return expand(src_path, 0);

    fn expand(src_path: &Path, depth: usize) -> anyhow::Result<String> {
        let code = fs::read_to_string(src_path)
            .with_context(|| format!("failed to read `{}`", src_path.display()))?;

        let file = syn::parse_file(&code).with_context(|| {
            format!("failed to parse the Rust code at `{}`", src_path.display())
        })?;

        let lines = code.lines().collect::<Vec<_>>();

        let mut path_attrs = vec![FixedBitSet::with_capacity(0); lines.len()];
        let mut mods = vec![SmallVec::<[_; 1]>::new(); lines.len()];

        if let (Some(dir), Some(file_stem)) = (src_path.parent(), src_path.file_stem()) {
            for item in file.items {
                if let Item::Mod(item_mod) = item {
                    if let Some(semi) = item_mod.semi {
                        let paths = item_mod
                            .attrs
                            .iter()
                            .flat_map(|a| a.parse_meta().map(|m| (a.span(), m)))
                            .flat_map(|(span, meta)| match meta {
                                Meta::NameValue(MetaNameValue { path, lit, .. }) if matches!(path.get_ident(), Some(i) if i == "path") => {
                                    Some((span, lit))
                                }
                                _ => None,
                            })
                            .flat_map(|(span, lit)| match lit {
                                Lit::Str(lit_str) => Some((span, lit_str.value())),
                                _ => None,
                            })
                            .next()
                            .map(|(s, p)| {
                                iter::once((Some(s), dir.join(p))).collect::<ArrayVec<[_; 2]>>()
                            })
                            .unwrap_or_else(|| {
                                let mut dir = dir.to_owned();
                                if !(depth == 0 || file_stem == "mod") {
                                    dir = dir.join(file_stem);
                                }
                                let mut paths = ArrayVec::new();
                                paths.push((None, dir.join(format!("{}.rs", item_mod.ident))));
                                paths.push((
                                    None,
                                    dir.join(item_mod.ident.to_string()).join("mod.rs"),
                                ));
                                paths
                            });

                        let (span, path) = paths
                            .iter()
                            .filter(|(_, p)| p.exists())
                            .exactly_one()
                            .map_err(|err| {
                            let paths = paths.iter().map(|(_, p)| p.to_str().unwrap()).format(", ");
                            match err.count() {
                                0 => anyhow!("None of the files exists: {{{}}}", paths),
                                _ => anyhow!("Multiple files exist: {{{}}}", paths),
                            }
                        })?;

                        if let Some(span) = *span {
                            macro_rules! insert(($i:expr, $range:expr) => {
                                let i = $i;
                                let line = &lines[i];
                                let path_attrs = &mut path_attrs[i];
                                if path_attrs.len() == 0 {
                                    path_attrs.grow(line.len());
                                }
                                path_attrs.insert_range($range);
                            });

                            let (start, end) = (span.start(), span.end());

                            if start.line == end.line {
                                insert!(start.line - 1, start.column..end.column);
                            } else {
                                insert!(start.line - 1, start.column..);
                                for i in start.line..end.line - 1 {
                                    insert!(i, ..);
                                }
                                insert!(end.line - 1, ..end.column);
                            }
                        }

                        let LineColumn { line, column } = semi.span().start();
                        mods[line - 1].push((column, expand(path, depth + 1)?));
                    }
                }
            }
        }

        lines
            .iter()
            .enumerate()
            .flat_map(|(i, line)| {
                let mut expanded = vec![b' '; 4 * depth];
                let mut mods = mods[i].iter().peekable();
                let mut modified = false;
                for (j, byte) in line.bytes().enumerate() {
                    if path_attrs[i][j] {
                        modified = true;
                    } else {
                        match mods.peek() {
                            Some((semi_col, content)) if *semi_col == j => {
                                mods.next();
                                expanded.extend_from_slice(b" {\n");
                                expanded.extend_from_slice(content.as_ref());
                                expanded.push(b'}');
                                modified = true;
                            }
                            _ => expanded.push(byte),
                        }
                    }
                }
                str::from_utf8(&expanded)
                    .with_context(|| format!("failed to expand at line {}: {:?}", i + 1, expanded))
                    .map(|s| Some(s.trim_end().to_owned()).filter(|s| !(modified && s.is_empty())))
                    .transpose()
            })
            .try_fold("".to_owned(), |mut acc, line| {
                acc += &line?;
                acc += "\n";
                Ok(acc)
            })
    }
}

trait WriteColorExt: WriteColor {
    fn status_with_color(
        &mut self,
        status: impl Display,
        message: impl Display,
        color: termcolor::Color,
    ) -> io::Result<()> {
        self.set_color(
            ColorSpec::new()
                .set_fg(Some(color))
                .set_bold(true)
                .set_reset(false),
        )?;
        write!(self, "{:>12}", status)?;
        self.reset()?;
        writeln!(self, " {}", message)?;
        self.flush()
    }
}

impl<W: WriteColor> WriteColorExt for W {}
