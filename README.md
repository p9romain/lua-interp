## Dependencies

In order to build the project, we need a few opam packages.
Using `opam`, run:

```
opam install dune menhir ppx_deriving ocaml-lsp-server ocamlformat-rpc
```

On the Rust side, a recent installation of cargo and rustc is needed.

## Usage

Building the project (both OCaml and Rust):

```
    make
```

Running all the tests (this uses all the .lua files in the `../lua/` directory as test files):

```
    make tests
```

**To have more information on a specific test** (for example `tests/01_print.lua`):

```
    make test TEST=tests/01_print.lua
```

To show the syntax tree corresponding to a mini-Lua program `prog.lua` after parsing by the OCaml parser (the AST in Rust should be equivalent), move to the `ocaml` directory then run:

```
    dune exec --display=quiet showast/showast.exe -- prog.lua
```

The *lua reference interpreter* source is in the `lua` directory. It can be built by typing `make refinterp`, and run using:

```
    lua/mini_lua.sh prog.lua
```
