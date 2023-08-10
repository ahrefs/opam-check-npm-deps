## v1.0.0 (2023-08-10)

- Initial version
- Read `depexts` field from all the opam files of the installed packages in the
  switch
- Check the depexts that include filters using the `npm-version` variable
  against the installed npm packages in `node_modules`
