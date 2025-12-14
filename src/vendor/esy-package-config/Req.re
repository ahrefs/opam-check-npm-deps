[@deriving ord]
type t = {
  name: string,
  spec: VersionSpec.t,
};

let show = ({ name, spec }) => name ++ "@" ++ VersionSpec.show(spec);

let to_yojson = req => `String(show(req));

let pp = (fmt, req) => Fmt.fmt("%s", fmt, show(req));

module Parse = {
  include Parse;

  let name =
    take_while1(
      fun
      | '@'
      | '/' => false
      | _ => true,
    );
  let opamPackageName = {
    let make = (scope, name) => `opam(scope ++ name);
    make <$> string("@opam/") <*> name;
  };

  let npmPackageNameWithScope = {
    let make = (scope, name) => `npm("@" ++ scope ++ "/" ++ name);
    make <$> char('@') *> name <*> char('/') *> name;
  };

  let npmPackageName = {
    let make = name => `npm(name);
    make <$> name;
  };

  let packageName =
    opamPackageName <|> npmPackageNameWithScope <|> npmPackageName;

  let parser = {
    let* name = packageName;
    peek_char
    >>= (
      fun
      | None => {
          let (name, spec) =
            switch (name) {
            | `npm(name) => (
                name,
                VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
              )
            | `opam(name) => (
                name,
                VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
              )
            };

          return({
            name,
            spec,
          });
        }
      | Some('@') => {
          let* () = advance(1);
          let* nextChar = peek_char;
          switch (nextChar, name) {
          | (None, _) =>
            let (name, spec) =
              switch (name) {
              | `npm(name) => (
                  name,
                  VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
                )
              | `opam(name) => (
                  name,
                  VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
                )
              };

            return({
              name,
              spec,
            });
          | (Some(_), `opam(name)) =>
            let* spec = VersionSpec.parserOpam;
            return({
              name,
              spec,
            });
          | (Some(_), `npm(name)) =>
            let* spec = VersionSpec.parserNpm;
            return({
              name,
              spec,
            });
          };
        }
      | _ => fail("cannot parse request")
    );
  };
};

let parse = Parse.(parse(parser));

let make = (~name, ~spec) => {
  name,
  spec,
};

module Set =
  Set.Make({
    type nonrec t = t;
    let compare = compare;
  });

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });
