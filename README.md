# cargo-expand-mods

[![CI](https://github.com/qryxip/cargo-expand-mods/workflows/CI/badge.svg)](https://github.com/qryxip/cargo-expand-mods/actions?workflow=CI)
[![codecov](https://codecov.io/gh/qryxip/cargo-expand-mods/branch/master/graph/badge.svg)](https://codecov.io/gh/qryxip/cargo-expand-mods/branch/master)
[![dependency status](https://deps.rs/repo/github/qryxip/cargo-expand-mods/status.svg)](https://deps.rs/repo/github/qryxip/cargo-expand-mods)
[![Crates.io](https://img.shields.io/badge/crates.io-not%20yet-inactive)](https://crates.io)
[![License](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-informational)](https://crates.io)

WIP

## Installation

```console
$ cargo install --git https://github.com/qryxip/cargo-expand-mods
```

## Usage

```console
$ cargo expand-mods --bin "$BIN" > ./main.rs
$ rustc ./main.rs --edition=2018 -C opt-level=2 -o ./a.out
$ ./a.out
```

```console
$ cargo expand-mods --help
cargo-expand-mods 0.0.0

USAGE:
    cargo expand-mods [OPTIONS]

OPTIONS:
        --features <FEATURES>...    [cargo] Space-separated list of features to activate
        --all-features              [cargo] Activate all available features
        --no-default-features       [cargo] Do not activate the `default` feature
        --manifest-path <PATH>      [cargo] Path to Cargo.toml
        --color <WHEN>              [cargo] TODO [possible values: auto, always, never]
        --frozen                    [cargo] Require Cargo.lock and cache are up to date
        --locked                    [cargo] Require Cargo.lock is up to date
        --offline                   [cargo] Run without accessing the network
    -p, --package <SPEC>            TODO
        --lib                       Expand this package's library
        --bin <NAME>                Expand the specified binary
        --example <NAME>            Expand the specified example
        --test <NAME>               Expand the specified test target
        --bench <NAME>              Expand the specified bench target
    -h, --help                      Prints help information
    -V, --version                   Prints version information
```

## License

Licensed under <code>[MIT](https://opensource.org/licenses/MIT) OR [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0)</code>.
