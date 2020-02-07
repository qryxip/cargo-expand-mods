use anyhow::{anyhow, Context as _};
use arrayvec::ArrayVec;
use cargo_metadata::{CargoOpt, MetadataCommand, Resolve, Target};
use itertools::Itertools as _;
use quote::quote;
use serde::Deserialize;
use structopt::clap::AppSettings;
use structopt::StructOpt;
use syn::token::Brace;
use syn::{Item, Lit, Meta, MetaNameValue};

use std::path::{Path, PathBuf};
use std::{fs, iter};

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

        /// TODO
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

    if color.is_some() || package.is_some() {
        todo!();
    }

    let src_path = {
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

        let id = metadata
            .resolve
            .as_ref()
            .and_then(|Resolve { root, .. }| root.as_ref())
            .ok_or_else(|| anyhow!("This manifest seems to be a virtual manifest"))
            .with_context(|| anyhow!("Could not determine the target package"))?;

        let package = metadata
            .packages
            .into_iter()
            .find(|p| p.id == *id)
            .unwrap_or_else(|| todo!());

        let default_run = fs::read_to_string(&package.manifest_path)
            .map_err(anyhow::Error::from)
            .and_then(|cargo_toml| toml::from_str::<CargoToml>(&cargo_toml).map_err(Into::into))
            .with_context(|| anyhow!("Could not read {}", package.manifest_path.to_str().unwrap()))?
            .package
            .and_then(|CargoTomlPackage { default_run }| default_run);

        package
            .targets
            .into_iter()
            .find(|Target { name, kind, .. }| {
                lib && (kind.contains(&"lib".to_owned()) || kind.contains(&"proc-macro".to_owned()))
                    || bin
                        .as_ref()
                        .map_or(false, |b| b == name && kind.contains(&"bin".to_owned()))
                    || example
                        .as_ref()
                        .map_or(false, |e| e == name && kind.contains(&"example".to_owned()))
                    || test
                        .as_ref()
                        .map_or(false, |t| t == name && kind.contains(&"test".to_owned()))
                    || bench
                        .as_ref()
                        .map_or(false, |b| b == name && kind.contains(&"bench".to_owned()))
                    || default_run
                        .as_ref()
                        .map_or(false, |d| d == name && kind.contains(&"bin".to_owned()))
            })
            .ok_or_else(|| anyhow!("Could not determine which target to expand"))?
            .src_path
    };

    let mut file = read_code(&src_path)?;
    expand_mods(&src_path, true, &mut file.items)?;

    println!("{}", quote!(#file));
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

fn expand_mods(current_path: &Path, is_start: bool, items: &mut [Item]) -> anyhow::Result<()> {
    for item in items {
        if let Item::Mod(item_mod) = item {
            if item_mod.content.is_none() {
                let dir = current_path.parent().unwrap_or_else(|| todo!());
                let file_stem = current_path.file_stem().unwrap_or_else(|| todo!());

                let paths = item_mod
                    .attrs
                    .iter()
                    .enumerate()
                    .flat_map(|(i, a)| a.parse_meta().map(|m| (Some(i), m)))
                    .flat_map(|(i, meta)| match meta {
                        Meta::NameValue(MetaNameValue { path, lit, .. })
                            if path.get_ident().map_or(false, |ident| ident == "path") =>
                        {
                            Some((i, lit))
                        }
                        _ => None,
                    })
                    .flat_map(|(i, lit)| match lit {
                        Lit::Str(lit_str) => Some((i, lit_str.value())),
                        _ => None,
                    })
                    .next()
                    .map(|(i, p)| iter::once((i, dir.join(p))).collect::<ArrayVec<[_; 2]>>())
                    .unwrap_or_else(|| {
                        let mut dir = dir.to_owned();
                        if !(is_start || file_stem == "mod") {
                            dir = dir.join(file_stem);
                        }
                        let mut paths = ArrayVec::new();
                        paths.push((None, dir.join(format!("{}.rs", item_mod.ident))));
                        paths.push((None, dir.join(item_mod.ident.to_string()).join("mod.rs")));
                        paths
                    });

                let (i, path) = paths
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

                if let Some(i) = *i {
                    item_mod.attrs.remove(i);
                }

                let syn::File {
                    attrs, mut items, ..
                } = read_code(&path)?;

                item_mod.attrs.extend(attrs);
                expand_mods(&path, false, &mut items)?;

                item_mod.content = Some((Brace::default(), items));
            }
            item_mod.semi = None;
        }
    }
    Ok(())
}

fn read_code(path: impl AsRef<Path>) -> anyhow::Result<syn::File> {
    let path = path.as_ref();
    fs::read_to_string(path)
        .map_err(anyhow::Error::from)
        .and_then(|code| syn::parse_file(&code).map_err(Into::into))
        .with_context(|| anyhow!("Could not read {}", path.display()))
}
