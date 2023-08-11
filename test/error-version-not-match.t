Errors when version does not match

  $ opam switch create . --empty | grep -v "to update the current shell environment"

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

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "^18.2.0",
  >     "react-dom": "^18.2.0"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Error: opam package "test.dev" requires npm package "react-dom" with constraint "^16.0.2", but the version installed found in file "$TESTCASE_ROOT/node_modules/react-dom/package.json" is "18.2.0"
  Error: opam package "test.dev" requires npm package "react" with constraint "^16.0.2", but the version installed found in file "$TESTCASE_ROOT/node_modules/react/package.json" is "18.2.0"
  [1]

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y
