## v4.1.0 (2025-12-23)

- Support opam 2.5
- Remove most vendored code, keep only the semver parser

## v4.0.1 (2025-09-30)

- Fix syntax error after reason/ppx_let upstream changes.

## v4.0.0 (2025-04-19)

- Support opam 2.4

## v3.0.1 (2024-09-24)

- Support opam 2.3

## v2.0.0 (2023-09-24)

- Support opam 2.2

## v1.0.0 (2023-08-10)

- Initial version
- Read `depexts` field from all the opam files of the installed packages in the
  switch
- Check the depexts that include filters using the `npm-version` variable
  against the installed npm packages in `node_modules`
