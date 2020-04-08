use anyhow::{anyhow, ensure, Context as _};
use arrayvec::ArrayVec;
use cargo_metadata::{CargoOpt, MetadataCommand, Package, Resolve, Target};
use duct::cmd;
use itertools::Itertools as _;
use proc_macro2::LineColumn;
use serde::Deserialize;
use smallvec::SmallVec;
use structopt::clap::AppSettings;
use structopt::StructOpt;
use syn::spanned::Spanned as _;
use syn::{Item, Lit, Meta, MetaNameValue};
use url::Url;

use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::{env, fs, iter, str};

#[derive(StructOpt, Debug)]
#[structopt(
    author,
    about,
    bin_name("cargo"),
    global_settings(&[AppSettings::DeriveDisplayOrder, AppSettings::UnifiedHelpMessage])
)]
enum Opt {
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

        /// [cargo] TODO
        #[structopt(
            long,
            value_name("WHEN"),
            case_insensitive(false),
            possible_values(&["auto", "always", "never"]),
        )]
        color: Option<String>,

        /// [cargo] Require Cargo.lock and cache are up to date
        #[structopt(long)]
        frozen: bool,

        /// [cargo] Require Cargo.lock is up to date
        #[structopt(long)]
        locked: bool,

        /// [cargo] Run without accessing the network
        #[structopt(long)]
        offline: bool,

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

fn main() -> anyhow::Result<()> {
    let Opt::ExpandMods {
        features,
        all_features,
        no_default_features,
        manifest_path,
        color,
        frozen,
        locked,
        offline,
        package,
        lib,
        bin,
        example,
        test,
        bench,
    } = Opt::from_args();

    if color.is_some() {
        todo!();
    }

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
        cmd.exec()?
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

        ensure!(output.status.success(), "{}", stderr);

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
            .and_then(|CargoTomlPackage { default_run }| default_run)
    } else {
        None
    };

    let mut targets = packages
        .into_iter()
        .flat_map(|p| p.targets.iter().map(move |t| (t, p)));

    let (Target { src_path, .. }, _) = if lib {
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

    let expanded = expand_mods(&src_path)?;

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

    let Output { stdout, .. } = cmd!(rustfmt_exe)
        .stdin_bytes(expanded)
        .stdout_capture()
        .run()?;

    println!("{}", str::from_utf8(&stdout)?.trim_end());
    Ok(())
}

#[derive(Deserialize, Debug)]
struct CargoToml {
    #[serde(default)]
    package: Option<CargoTomlPackage>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
struct CargoTomlPackage {
    #[serde(default)]
    default_run: Option<String>,
}

fn expand_mods(src_path: &Path) -> anyhow::Result<String> {
    return expand(src_path, 0);

    fn expand(src_path: &Path, depth: usize) -> anyhow::Result<String> {
        let code = fs::read_to_string(src_path)
            .with_context(|| format!("failed to read `{}`", src_path.display()))?;

        let file = syn::parse_file(&code).with_context(|| {
            format!("failed to parse the Rust code at `{}`", src_path.display())
        })?;

        let mut lines = code.lines().map(Cow::from).collect::<Vec<_>>();
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
                            let (start, end) = (span.start(), span.end());
                            if start.line == end.line {
                                lines[start.line - 1] = lines[start.line - 1]
                                    .chars()
                                    .enumerate()
                                    .map(|(i, c)| {
                                        if (start.column..=end.column).contains(&i) {
                                            ' '
                                        } else {
                                            c
                                        }
                                    })
                                    .collect::<String>()
                                    .into();
                            } else {
                                todo!();
                            }
                        }

                        let LineColumn { line, column } = semi.span().start();
                        mods[line - 1].push((column, expand(path, depth + 1)?));
                    }
                }
            }
        }

        Ok(lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                let mut expanded = itertools::repeat_n(' ', 4 * depth).collect::<String>();
                let mut mods = mods[i].iter().peekable();
                for (j, ch) in line.chars().enumerate() {
                    match mods.peek() {
                        Some((semi_col, content)) if *semi_col == j => {
                            mods.next();
                            expanded += " {\n";
                            expanded += content;
                            expanded += "}";
                        }
                        _ => expanded.push(ch),
                    }
                }
                if expanded.len() == 4 * depth {
                    expanded.clear(); // trim spaces
                }
                expanded += "\n";
                expanded
            })
            .join(""))
    }
}
