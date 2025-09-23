# KaTeX-rs

[![Crates.io](https://img.shields.io/crates/v/katex-rs.svg)](https://crates.io/crates/katex-rs)
[![Documentation](https://docs.rs/katex-rs/badge.svg)](https://docs.rs/katex-rs)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**KaTeX-rs** is a Rust implementation of [KaTeX](https://github.com/KaTeX/KaTeX), providing fast mathematical typesetting capabilities, not limited to Javascript environments.

## Project Introduction

KaTeX-rs is a working in progress Rust port of KaTeX (a fast mathematical typesetting library). It converts LaTeX mathematical expressions into HTML and MathML formats, supporting server-side rendering, command-line tools, and WebAssembly environments.

This project is based on KaTeX's commit [9fb63136e680715ad83c119366f6f697105d2c55](https://github.com/KaTeX/KaTeX/commit/9fb63136e680715ad83c119366f6f697105d2c55).

- [x] Basic parsing and rendering
- [x] Unit and integration tests
- [x] Offline rendering tests
- [x] Compatible with `no-std` and `wasm` target
- [ ] Fully consistent with KaTeX result

### Feature Flags

- `backtrace`: Enables backtrace support for better error diagnostics
- `wasm`: Enables WebAssembly support

### Prerequisites for development

For development, ensure you have fully checked out the repository with all submodules and Git LFS files:
```
git lfs install && git lfs pull
git submodule update --init --recursive
```

For testing, you will need to have `node` and `npm` installed and available in your `PATH`.
You also need nightly Rust toolchain for some linting and testing features.
`wasm-pack` would automatically install required toolchain for wasm target.

Install `wasm-pack` and `cargo-nextest` (for running tests) either via the script:
```bash
rustup default nightly
curl -LsSf https://get.nexte.st/latest/linux | tar zxf - -C ${CARGO_HOME:-~/.cargo}/bin
curl https://drager.github.io/wasm-pack/installer/init.sh -sSf | sh
```

(Or with cargo)
```bash
cargo install --locked cargo-nextest
cargo install wasm-pack
```

### Unit tests
```bash
cargo nextest run --no-fail-fast
```

### Screenshot tests

The script will automatically build the wasm and run the screenshot tests.

```bash
pushd ./tests/screenshotter && npm install && popd
node tests/screenshotter/run.js
```

## Compatibility

- **Rust**: 1.70+ (Testing and Linting needs nightly)
- **WebAssembly**: Supports all modern browsers
- **KaTeX**: Fully compatible with the original KaTeX JavaScript version

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.