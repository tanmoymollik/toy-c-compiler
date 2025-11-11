# Toy C Compiler in OCaml #

This repository contains my personal implementation of a C compiler, following
along with the book "Writing a C Compiler" by Nora Sandler. 📚

Currently this implementation passes tests upto chapter 5 with bonus parts.

My first implementation was written in Rust because I had recently learnt it and
wanted to try it out on something. But then I pivoted to OCaml because I found
it better suited for writing a compiler.

## Getting Started ##

My compiler uses intel x64 assembly syntax. Thus I found nasm is better suited
to assemble the assembly file produced by the compiler. The book uses gcc for
this step however. This mean that the instructions produced by this compiiler
and the book is different (src and dst are swapped). In order to use this
compiler `gcc` and `nasm` must be installed. To understand the differences in
assembly produced by the book and this repo look at
[emitter internals](lib/emitter.ml).

This repo contains a single driver binary. To build it run:

```sh
dune build
```

Driver binary uses `gcc` to pre-process the file, then compile the pre-processed
file, assemble it using `nasm` and then use `gcc` to link the generated object
file. This binary also takes an optional stage option as demanded in the book to
test intermediate stages:
- `--lex`
- `--parse`
- `--codegen`
- `-S`

```sh
dune exec c_compiler -- --codegen test.c
```

This repository entirely depends on the tests provided by
[writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests/tree/main)
for validation. I might add some tests of my own later.

This compiler support development on both macos and linux. Currently I am
working on macos, so by default the compiler emits assembly code suitable for
macos. For linux development, set `platform` to `Lib.Platform.Linux` in
[driver binary](bin/driver.ml).

## Implementation Notes ##
Loop labels are set as dummy when parsing and resolved in semantic analysis pass.
Case and Default statements converted to Label statements in semantic analysis pass.