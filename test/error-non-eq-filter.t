Errors when using npm-version without an equality filter `=`

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
  >   ["react"] {npm-version >= "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  -> installed test.dev

  $ opam-check-npm-deps
  Warning: package test.dev includes an invalid npm-version constraint which does not use equality in its formula: npm-version >= "^16.0.2".
  To fix the issue, use an equality formula, e.g. {npm-version = "^1.0.0"}

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y
