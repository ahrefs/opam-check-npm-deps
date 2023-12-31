Fmt — OCaml Format pretty-printer combinators
-------------------------------------------------------------------------------
%%VERSION%%

Fmt exposes combinators to devise `Format` pretty-printing functions.

Fmt depends only on the OCaml standard library. The optional `Fmt_tty`
library that allows to setup formatters for terminal color output
depends on the Unix library. The optional `Fmt_cli` library that
provides command line support for Fmt depends on [`Esy_cmdliner`][esy_cmdliner].

Fmt is distributed under the ISC license.

[esy_cmdliner]: http://erratique.ch/software/esy_cmdliner

Home page: http://erratique.ch/software/fmt  

## Installation

Fmt can be installed with `opam`:

    opam install fmt
    opam install base-unix esy_cmdliner fmt # Install all optional libraries

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][doc]
and there is a generated version in the `doc` directory of the
distribution.

[doc]: http://erratique.ch/software/fmt/doc/
