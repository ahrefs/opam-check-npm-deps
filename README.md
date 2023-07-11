# opam-check-npm-deps

Provides the `opam check-npm-deps` command, which given an opam switch, gathers
all the depexts belonging to the npm platform and their version constraints, and
checks the `node_modules` folder to see if the constraints are satisfied.

## Usage

Install:
```shell
$ opam pin add opam-check-npm-deps.dev git+https://github.com/jchavarri/opam-check-npm-deps.git#main
```

Run check:
```shell
$ opam-check-npm-deps
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
- `make watch`: watch for the filesystem and have Dune rebuild on every
  change
