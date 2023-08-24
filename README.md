# opam-check-npm-deps

An opam plugin to allow using the `depexts` field to define requirements on
[npm](https://www.npmjs.com/) packages.

This plugin aims to solve a simple limitation: how to allow libraries written
for either [Melange](https://melange.re/) or
[Js_of_ocaml](http://ocsigen.org/js_of_ocaml) to define dependencies to
JavaScript packages. It addresses the problem by enabling library authors to
leverage the [depexts
field](https://opam.ocaml.org/doc/Manual.html#opamfield-depexts) in opam files
to define dependencies on [npm](https://www.npmjs.com/) packages.

## For bindings authors: defining constraints

Constraints are defined by adding an entry to `depexts` with the npm package
name as the "system package" and an equality formula that matches the
`npm-version` variable to a version range. This range can be defined using the
same format as the `dependencies` field in a
[package.json](https://docs.npmjs.com/cli/v8/configuring-npm/package-json#dependencies)
file [^1].

For example, the `reason-react` bindings can define their dependency on the
`react` npm package like this:

```plaintext
depexts: [
  ["react"] {npm-version = "^16.0.0 || ^17.0.0"}
  ["react-dom"] {npm-version = "^16.0.0 || ^17.0.0"}
]
```

Or, to simplify:

```plaintext
depexts: [
  ["react" "react-dom"] {npm-version = "^16.0.0 || ^17.0.0"}
]
```

## For library users: checking npm dependencies status

Users can check the state of the npm dependencies by running the plugin:

```shell
$ opam-check-npm-deps
Ok: opam package "test.dev" requires the npm package "react" with constraint "^16.0.0 || ^17.0.0", installed version: "17.0.2"
Ok: opam package "test.dev" requires the npm package "react-dom" with constraint "^16.0.0 || ^17.0.0", installed version: "17.0.2"
```

The plugin will provide information when errors occur:

```shell
$ opam-check-npm-deps
Error: opam package "test.dev" requires the npm package "react" with constraint "^16.0.0 || ^17.0.0", but the installed version found in the "node_modules/react/package.json" file is "18.2.0"
Error: opam package "test.dev" requires the npm package "react-dom" with constraint "^16.0.0 || ^17.0.0", but the installed version found in the "node_modules/react-dom/package.json" file is "18.2.0"
```

When running the plugin, it checks the current opam switch to read all `depexts`
fields that use the `npm-version` variable, and then reads the `node_modules`
folder to determine if the constraints are satisfied.

If you don't want to the command to fail when errors are found, use the
`--dry-run` flag:

```shell
$ opam-check-npm-deps --dry-run
```

## Contributing

```shell
$ make init

$ make watch
```

### Commands

You can see all available commands by running `make help` or just `make`. Here
are a few of the most useful ones:

- `make init`: set up opam local switch and download OCaml, Melange and
  JavaScript dependencies
- `make install`: install OCaml dependencies
- `make watch`: watch for the filesystem and have Dune rebuild on every change

[^1]: Special thanks to the esy authors and contributors, as the plugin reuses
    many of esy's libraries to analyze constraints in a format compatible with
    `package.json` files.
