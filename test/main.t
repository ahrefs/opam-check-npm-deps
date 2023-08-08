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
  Ok: opam package "test.dev" requires npm package: "react-dom" with constraint "^16.0.2", version installed: "16.14.0"
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.2", version installed: "16.14.0"

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

Errors when version does not match

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
  Error: opam package "test.dev" requires npm package "react-dom" with constraint "^16.0.2", but the version installed is "18.2.0"
  Error: opam package "test.dev" requires npm package "react" with constraint "^16.0.2", but the version installed is "18.2.0"

  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev

Errors when node_module folder is not found

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

  $ opam pin remove -y test | grep "removed"
  -> removed   test.dev
