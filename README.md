Z3x — Simple Z3 Bindings
-------------------------------------------------------------------------------
%%VERSION%%

Z3x is TODO

Z3x is distributed under the ISC license.

Homepage: https://github.com/zshipko/Z3x

## Installation

Z3x can be installed with `opam`:

    opam install Z3x

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
Z3x`.

[doc]: https://zshipko.github.io/Z3x/doc

## Sample programs

If you installed Z3x with `opam` sample programs are located in
the directory `opam var Z3x:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test
