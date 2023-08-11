Errors when node_module folder is not found

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
  > homepage: "https://github.com/test/project"
  > bug-reports: "https://github.com/test/project/issues"
  > depexts: [
  >   ["react"] {npm-version = "^16.0.2"}
  >   ["react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  -> installed test.dev

  $ rm -rf node_modules

  $ opam-check-npm-deps
  Error: opam package "test.dev" requires npm package "react-dom" with constraint "^16.0.2", but file "$TESTCASE_ROOT/node_modules/react-dom/package.json" can not be found
  Error: opam package "test.dev" requires npm package "react" with constraint "^16.0.2", but file "$TESTCASE_ROOT/node_modules/react/package.json" can not be found
  [1]

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y
