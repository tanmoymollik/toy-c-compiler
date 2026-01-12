# Toy C Compiler in OCaml #

This repository contains my personal implementation of a C compiler, following
along with the book "Writing a C Compiler" by Nora Sandler. 📚

Currently this implementation passes tests upto chapter 16 with extra credits.
This includes:
- binary, unary and logical expressions
- if/else, switch, goto/label
- for, while, do/while loops
- local, static and extern variables
- local, static and extern functions
- int, char, long, double, unsigned, signed, pointers and arrays
- void*, struct **[Work in progress]**

I also implemented chapter 19 (tacky optimizations) and chapter 20 (register
allocation) for integer types. After finishing chapters 17-18 I will add support
for other types.

My first implementation was written in Rust because I had recently learnt it and
wanted to try it out on something. But then I pivoted to OCaml because I found
it better suited for writing a compiler.

## Getting Started ##

My compiler uses intel x64 assembly syntax. Thus I found nasm is better suited
to assemble the assembly file produced by the compiler. The book uses gcc for
this step however. This mean that the instructions produced by this compiiler
and the book is different (src and dst are swapped). In order to use this
compiler `gcc` and `nasm` must be installed. 

*UPDATE:* I ended up implementing gnu style assembly emission too in the end.
The optimization and register allocation tests for chapter 19 and 20 require gnu
style assembly.

This repo contains a single driver binary. To build it run:

```sh
dune build
```

Driver binary uses `gcc` to pre-process the file, then compile the pre-processed
file, assemble it using `nasm` and then use `gcc` to link the generated object
file. This binary also takes optional stage arguments mandated by the book to run
the test suite. I also implemented a few other command line flags for development.
To see the full list of options available run:

```sh
dune exec c_compiler -- -h
```

To simply compile a file run:

```sh
dune exec c_compiler -- my_file.c
```

*NOTE:* The tacky optimization and register allocation passes require command line
flags to be specified. To know which flags will enable them run the command to list
all options.

## Tests ##

This repository entirely depends on the tests provided by
[writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests/tree/main)
for validation for now. The test suite is pretty comprehensive.

I created a [fork](https://github.com/tanmoymollik/toy-c-compiler-tests.git) of
my own to add support for riscv architecture. The fork supports tests upto chapter
18 for riscv. But chapter 19 and 20 won't work because they are tied to x86_64
architecture.

## Implementation Notes ##
- This implementation lexes and parses at the same time. So just stopping after lex
  stage is not possible. --lex option does nothing.
- Implementation assumes x64 architecture. Both int and long in c are parsed as ocaml int.
- Loop labels are set as dummy when parsing and resolved in semantic analysis pass.
- Case and Default statements are converted to Label statements in semantic analysis pass.
- **TBD**