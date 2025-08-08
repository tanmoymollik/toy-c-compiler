# Toy C Compiler in Rust #

This repository contains my personal implementation of a C compiler, following along with the book "Writing a C Compiler" by Nora Sandler. 📚

Currently this implementation only passes the tests for chapter 1.

My implementation is written in Rust because I recently learnt it and wanted to try it out on something.

## Getting Started ##

This repo contains 2 binaries. A simple compiler binary and a compiler driver binary. To build both binaries:

```sh
cargo build
```

1. Compiler binary takes a filename as input and produces `filename.s` assembly file.

    ```sh
    cargo run --bin compiler -- test.c
    ```

2. Driver binary uses `gcc` to pre-process the file, then compile the pre-processes file and then use `gcc` to assemble and link the generated assembly file. This binary also takes an optional stage option as demanded in the book to test intermediate stages:
    - `--lex`
    - `--parse`
    - `--codegen`
    - `-S`

    ```sh
    cargo run --bin compiler -- --codegen test.c
    ```

This repository entirely depends on the tests provided by [writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests/tree/main) for validation. I might add some tests of my own later.

This compiler support development on both macos and linux. Currently I am working on macos, so by default the compiler emits assembly code suitable for macos. For linux development, use linux as default in the feature section in [Cargo.toml](src/Cargo.toml).

## Notes ##

- One of the tests in chapter 1 contains comments. Though this wasn't metioned in chapter 1 that we would need to handle comments.