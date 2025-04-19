All tests added in the same file so that there are no opam lock issues
and -j 1 is not needed (because programs like dune-release will call @runtest directly)

When --dry-run is used, exit code is 0

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
  >   ["react"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "18.2.0"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps --dry-run
  Error: opam package "test.dev" requires npm package "react" with constraint "^16.0.2", but the version installed found in file "$TESTCASE_ROOT/node_modules/react/package.json" is "18.2.0"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y

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
  > license: "MIT"
  > homepage: "https://github.com/test/project"
  > bug-reports: "https://github.com/test/project/issues"
  > depexts: [
  >   ["react"] {npm-version = "^16.0.2"}
  >   ["react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ rm -rf node_modules

  $ opam-check-npm-deps
  Error: opam package "test.dev" requires npm package "react-dom" with constraint "^16.0.2", but file "$TESTCASE_ROOT/node_modules/react-dom/package.json" can not be found
  Error: opam package "test.dev" requires npm package "react" with constraint "^16.0.2", but file "$TESTCASE_ROOT/node_modules/react/package.json" can not be found
  [1]

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y

Errors when using npm-version without an equality filter `=`

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
  >   ["react"] {npm-version >= "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ opam-check-npm-deps
  Warning: package test.dev includes an invalid npm-version constraint which does not use equality in its formula: npm-version >= "^16.0.2".
  To fix the issue, use an equality formula, e.g. {npm-version = "^1.0.0"}

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y

Errors when version does not match

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
  >   ["react"] {npm-version = "^16.0.2"}
  >   ["react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "18.2.0",
  >     "react-dom": "18.2.0"
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

Supports package with more than one depext

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
  >   ["react"] {npm-version = "^16.0.2"}
  >   ["react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "16.14.0",
  >     "react-dom": "16.14.0"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Ok: opam package "test.dev" requires npm package: "react-dom" with constraint "^16.0.2", version installed: "16.14.0"
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.2", version installed: "16.14.0"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y

Supports having more than one npm package in the syspkg list

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
  >   ["react" "react-dom"] {npm-version = "^16.0.2"}
  > ]
  > EOF

  $ opam pin add -y test.dev . | grep "installed"
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "16.14.0",
  >     "react-dom": "16.14.0"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.2", version installed: "16.14.0"
  Ok: opam package "test.dev" requires npm package: "react-dom" with constraint "^16.0.2", version installed: "16.14.0"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y

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
  [NOTE] Package test does not exist in opam repositories registered in the current switch.
  -> installed test.dev

  $ cat > package.json <<EOF
  > {
  >   "license": "MIT",
  >   "dependencies": {
  >     "react": "17.0.2"
  >   }
  > }
  > EOF

  $ yarn install --silent

  $ opam-check-npm-deps
  Ok: opam package "test.dev" requires npm package: "react" with constraint "^16.0.0 || ^17.0.0", version installed: "17.0.2"

  $ opam switch remove -y .
  Switch $TESTCASE_ROOT and all its packages will be wiped. Are you sure? [Y/n] y
