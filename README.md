# opam-check-npm-deps

An opam plugin to allow using the `depexts` field to define requirements on
[npm](https://www.npmjs.com/) packages.

When running the plugin, it checks the current opam switch to read all `depexts`
fields that use the `npm-version` variable, and then reads the `node_modules`
folder to see if the constraints are satisfied.

## Defining constraints

For example, the `reason-react` bindings can define its dependency on the
`react` npm package like this:

```
depexts: [
  ["react"] {npm-version = "^16.0.2"}
  ["react-dom"] {npm-version = "^16.0.2"}
]
```

## Usage

Install:
```shell
$ opam pin add opam-check-npm-deps.dev git+https://github.com/jchavarri/opam-check-npm-deps.git#main
```

Check the dependencies by running the plugin:
```shell
$ opam-check-npm-deps
```

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
