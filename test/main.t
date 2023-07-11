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
  pkg: test.dev, depexts: { react, react-dom }
  
  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev
