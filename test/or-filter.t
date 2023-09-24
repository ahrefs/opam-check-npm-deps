Supports || filters in the npm-version string

  $ OPAMNOENVNOTICE="true" opam switch create . --empty

  $ cat > test.opam <<EOF
  > opam-version: "2.0"
  > name: "test"
  > synopsis: "One-line description"
  > description: """
  > Longer description
  > """
  > maintainer: "Name <email>"
  > authors: "Name <email>"
  > license: "MIT"
  > homepage: "https://github.com/test/project"
  > bug-reports: "https://github.com/test/project/issues"
  > depexts: [
  >   ["react"] {npm-version = "^16.0.0 || ^17.0.0"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "^17.0.0"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.0 || ^17.0.0", version installed: "17.0.2"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [y/n] y
