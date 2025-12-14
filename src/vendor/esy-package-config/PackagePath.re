module String = Astring.String;

[@deriving ord]
type t = (list(segment), string)
and segment =
  | Pkg(string)
  | AnyPkg;

let show = ((path, pkg)) =>
  switch (path) {
  | [] => pkg
  | path =>
    let path =
      path
      |> List.map(
           ~f=
             fun
             | Pkg(name) => name
             | AnyPkg => "**",
         )
      |> String.concat(~sep="/");
    path ++ "/" ++ pkg;
  };

let pp = (fmt, v) => Fmt.pf(fmt, "%s", show(v));

let parse = v => {
  let parts = String.cuts(~empty=true, ~sep="/", v);
  let f = ((parts, scope), segment) =>
    switch (segment) {
    | "" => Error("invalid package path: " ++ v)
    | segment =>
      switch (segment.[0], segment, scope) {
      | ('@', _, None) => [@implicit_arity] Ok(parts, Some(segment))
      | ('@', _, Some(_)) => Error("invalid package path: " ++ v)
      | (_, "**", None) => [@implicit_arity] Ok([AnyPkg, ...parts], None)
      | (_, _, None) =>
        [@implicit_arity] Ok([Pkg(segment), ...parts], None)
      | (_, "**", Some(_)) => Error("invalid package path: " ++ v)
      | (_, _, Some(scope)) =>
        let pkg = scope ++ "/" ++ segment;
        [@implicit_arity] Ok([Pkg(pkg), ...parts], None);
      }
    };

  switch (Result.List.foldLeft(~f, ~init=([], None), parts)) {
  | Error(err) => Error(err)
  | [@implicit_arity] Ok([], None)
  | [@implicit_arity] Ok(_, Some(_))
  | [@implicit_arity] Ok([AnyPkg, ..._], None) =>
    Error("invalid package path: " ++ v)
  | [@implicit_arity] Ok([Pkg(pkg), ...path], None) =>
    [@implicit_arity] Ok(List.rev(path), pkg)
  };
};

let to_yojson = v => `String(show(v));
let of_yojson =
  fun
  | `String(v) => parse(v)
  | _ => Error("expected string");
