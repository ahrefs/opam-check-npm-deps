Supports having more than one npm package in the syspkg list

  $ opam switch create . --empty

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
  >   ["react" "react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "^16.0.2",
  >     "react-dom": "^16.0.2"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.2", version installed: "16.14.0"
  Ok: opam package "test.dev" requires npm package: "react-dom" with constraint "^16.0.2", version installed: "16.14.0"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y
