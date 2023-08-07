Supports package with more than one depext

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

  $ opam-check-npm-deps
  Found the following npm dependencies in opam files:
  opam pkg: test.dev, npm pkgs: { react-dom }, constraint: ^16.0.2
  opam pkg: test.dev, npm pkgs: { react }, constraint: ^16.0.2

  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev

Supports package one depext, where there is more than one npm package in the syspkg list

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

  $ opam-check-npm-deps
  Found the following npm dependencies in opam files:
  opam pkg: test.dev, npm pkgs: { react, react-dom }, constraint: ^16.0.2

  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev

Errors when using npm-version without an equality filter `=`

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
  Warning: package test.dev includes an invalid npm-version constraint which does not use equality in its formula: npm-version >= "^16.0.2"

  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev
