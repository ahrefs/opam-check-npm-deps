module Version = {
  type t = {
    major: int,
    minor: int,
    patch: int,
    prerelease,
    build,
  }
  and prerelease = list(segment)
  and build = list(string)
  and segment =
    | W(string)
    | N(int);

  let ppSegment = fmt =>
    fun
    | W(v) => Fmt.string(fmt, v)
    | N(v) => Fmt.int(fmt, v);

  let ppPrerelease = Fmt.(list(~sep=any("."), ppSegment));

  let ppBuild = Fmt.(list(~sep=any("."), string));

  let compareSegment = (a, b) =>
    switch (a, b) {
    | (N(_), W(_)) => (-1)
    | (W(_), N(_)) => 1
    | (N(a), N(b)) => compare(a, b)
    | (W(a), W(b)) => String.compare(a, b)
    };

  let make = (~prerelease=[], ~build=[], major, minor, patch) => {
    major,
    minor,
    patch,
    prerelease,
    build,
  };

  let show = v => {
    let prelease =
      switch (v.prerelease) {
      | [] => ""
      | v => Format.asprintf("-%a", ppPrerelease, v)
      };

    let build =
      switch (v.build) {
      | [] => ""
      | v => Format.asprintf("+%a", ppBuild, v)
      };

    Format.asprintf(
      "%i.%i.%i%s%s",
      v.major,
      v.minor,
      v.patch,
      prelease,
      build,
    );
  };

  let pp = (fmt, v) => Fmt.pf(fmt, "%s", show(v));

  let majorMinorPatch = v => Some((v.major, v.minor, v.patch));

  let prerelease = v =>
    switch (v.prerelease, v.build) {
    | ([], []) => false
    | (_, _) => true
    };

  let stripPrerelease = v => {...v, prerelease: [], build: []};

  module Parse = {
    open Re;
    let dot = char('.');
    let dash = char('-');
    let plus = char('+');
    let section = group(rep1(digit));
    let prereleaseChar = alt([alnum, char('-'), char('.')]);
    let prerelease = opt(seq([opt(dash), group(rep1(prereleaseChar))]));
    let build = opt(seq([opt(plus), group(rep1(prereleaseChar))]));
    let prefix = rep(alt([char('v'), char('=')]));

    let version3 =
      compile(
        seq([
          bos,
          prefix,
          section,
          dot,
          section,
          dot,
          section,
          prerelease,
          build,
          eos,
        ]),
      );

    let version2 =
      compile(
        seq([bos, prefix, section, dot, section, prerelease, build, eos]),
      );

    let version1 =
      compile(seq([bos, prefix, section, prerelease, build, eos]));

    let prerelaseAndBuild = compile(seq([bos, prerelease, build, eos]));
  };

  let intAtExn = (n, m) => {
    let v = Re.Group.get(m, n);
    int_of_string(v);
  };

  let optStrignAt = (n, m) =>
    switch (Re.Group.get(m, n)) {
    | exception Not_found => None
    | "" => None
    | v => Some(v)
    };

  let parsePrerelease = v =>
    v
    |> String.split_on_char('.')
    |> List.map(v =>
         try(N(int_of_string(v))) {
         | _ => W(v)
         }
       );

  let parseBuild = v => String.split_on_char('.', v);

  let parsePrerelaseAndBuild = v =>
    switch (Re.exec_opt(Parse.prerelaseAndBuild, v)) {
    | Some(m) =>
      let prerelease =
        switch (optStrignAt(1, m)) {
        | Some(v) => parsePrerelease(v)
        | None => []
        };

      let build =
        switch (optStrignAt(2, m)) {
        | Some(v) => parseBuild(v)
        | None => []
        };

      [@implicit_arity] Ok(prerelease, build);
    | None =>
      let msg = Printf.sprintf("unable to parse prerelease part: %s", v);
      Error(msg);
    };

  let parse = version =>
    switch (Re.exec_opt(Parse.version3, version)) {
    | Some(m) =>
      let major = intAtExn(1, m);
      let minor = intAtExn(2, m);
      let patch = intAtExn(3, m);
      let prerelease =
        switch (optStrignAt(4, m)) {
        | Some(v) => parsePrerelease(v)
        | None => []
        };

      let build =
        switch (optStrignAt(5, m)) {
        | Some(v) => parseBuild(v)
        | None => []
        };

      Ok({major, minor, patch, prerelease, build});
    | None =>
      switch (Re.exec_opt(Parse.version2, version)) {
      | Some(m) =>
        let major = intAtExn(1, m);
        let minor = intAtExn(2, m);
        let prerelease =
          switch (optStrignAt(3, m)) {
          | Some(v) => parsePrerelease(v)
          | None => []
          };

        let build =
          switch (optStrignAt(4, m)) {
          | Some(v) => parseBuild(v)
          | None => []
          };

        Ok({major, minor, patch: 0, prerelease, build});
      | None =>
        switch (Re.exec_opt(Parse.version1, version)) {
        | Some(m) =>
          let major = intAtExn(1, m);
          let prerelease =
            switch (optStrignAt(2, m)) {
            | Some(v) => parsePrerelease(v)
            | None => []
            };

          let build =
            switch (optStrignAt(3, m)) {
            | Some(v) => parseBuild(v)
            | None => []
            };

          Ok({major, minor: 0, patch: 0, prerelease, build});
        | None =>
          let msg = Printf.sprintf("invalid semver version: '%s'", version);
          Error(msg);
        }
      }
    };

  let parseExn = v =>
    switch (parse(v)) {
    | Ok(v) => v
    | Error(err) => raise(Invalid_argument(err))
    };

  let parser = {
    let p = parse;
    open Angstrom;
    let* input = take_while1(_ => true);
    switch (p(input)) {
    | Ok(v) => return(v)
    | Error(msg) => fail(msg)
    };
  };

  let comparePrerelease = (a: list(segment), b: list(segment)) => {
    let rec compare = (a, b) =>
      switch (a, b) {
      | ([], []) => 0
      | ([], _) => (-1)
      | (_, []) => 1
      | ([x, ...xs], [y, ...ys]) =>
        switch (compareSegment(x, y)) {
        | 0 => compare(xs, ys)
        | v => v
        }
      };

    switch (a, b) {
    | ([], []) => 0
    | ([], _) => 1
    | (_, []) => (-1)
    | (a, b) => compare(a, b)
    };
  };

  let compareBuild = (a: list(string), b: list(string)) => {
    let rec compare = (a, b) =>
      switch (a, b) {
      | ([], []) => 0
      | ([], _) => (-1)
      | (_, []) => 1
      | ([x, ...xs], [y, ...ys]) =>
        switch (String.compare(x, y)) {
        | 0 => compare(xs, ys)
        | v => v
        }
      };

    switch (a, b) {
    | ([], []) => 0
    | ([], _) => 1
    | (_, []) => (-1)
    | (a, b) => compare(a, b)
    };
  };

  let compare = (a, b) =>
    switch (a.major - b.major) {
    | 0 =>
      switch (a.minor - b.minor) {
      | 0 =>
        switch (a.patch - b.patch) {
        | 0 =>
          switch (comparePrerelease(a.prerelease, b.prerelease)) {
          | 0 => compareBuild(a.build, b.build)
          | v => v
          }
        | v => v
        }
      | v => v
      }
    | v => v
    };
};

module Constraint = VersionBase.Constraint.Make(Version);

module Formula = {
  include VersionBase.Formula.Make(Version, Constraint);

  let any: DNF.t = ([[Constraint.ANY]]: DNF.t);

  module Parser = {
    let sliceToEnd = (text, num) =>
      String.sub(text, num, String.length(text) - num);

    let isint = v =>
      try(
        {
          ignore(int_of_string(v));
          true;
        }
      ) {
      | _ => false
      };

    let parsePrerelaseAndBuild = v =>
      switch (Version.parsePrerelaseAndBuild(v)) {
      | Ok(v) => v
      | Error(err) => failwith(err)
      };

    let exactPartial = partial =>
      switch (partial) {
      | `AllStar => failwith("* cannot be compared")
      | `MajorStar(major) => Version.make(major, 0, 0)
      | `MinorStar(major, minor) => Version.make(major, minor, 0)
      | `Major(major, prerelease, build) =>
        Version.make(~prerelease, ~build, major, 0, 0)
      | `Minor(major, minor, prerelease, build) =>
        Version.make(~prerelease, ~build, major, minor, 0)
      | `Patch(major, minor, patch, prerelease, build) =>
        Version.make(~prerelease, ~build, major, minor, patch)
      | `Raw(prerelease, build) => Version.make(~prerelease, ~build, 0, 0, 0)
      };

    let parsePartial = version => {
      let version = version.[0] == '=' ? sliceToEnd(version, 1) : version;

      let version = version.[0] == 'v' ? sliceToEnd(version, 1) : version;

      let parts = String.split_on_char('.', version);
      switch (parts) {
      | ["*" | "x" | "X", ..._rest] => `AllStar
      | [major, "*" | "x" | "X", ..._rest] when isint(major) =>
        `MajorStar(int_of_string(major))
      | [major, minor, "*" | "x" | "X", ..._rest]
          when isint(major) && isint(minor) =>
        `MinorStar((int_of_string(major), int_of_string(minor)))

      | _ =>
        let rx =
          Str.regexp(
            {|^ *\([0-9]+\)\(\.\([0-9]+\)\(\.\([0-9]+\)\)?\)?\(\([-+~][a-z0-9\.]+\)\)?|},
          );
        switch (Str.search_forward(rx, version, 0)) {
        | exception Not_found => `Raw(parsePrerelaseAndBuild(version))
        | _ =>
          let major = int_of_string(Str.matched_group(1, version));
          let (prerelease, build) =
            switch (Str.matched_group(7, version)) {
            | exception Not_found =>
              let last = Str.match_end();
              if (last < String.length(version)) {
                parsePrerelaseAndBuild(sliceToEnd(version, last));
              } else {
                ([], []);
              };
            | text => parsePrerelaseAndBuild(text)
            };
          switch (Str.matched_group(3, version)) {
          | exception Not_found => `Major((major, prerelease, build))
          | minor =>
            let minor = int_of_string(minor);
            switch (Str.matched_group(5, version)) {
            | exception Not_found => `Minor((major, minor, prerelease, build))
            | patch =>
              `Patch((major, minor, int_of_string(patch), prerelease, build))
            };
          };
        };
      };
    };

    let parsePrimitive = item =>
      switch (item.[0]) {
      | '=' =>
        Constraint.EQ(exactPartial(parsePartial(sliceToEnd(item, 1))))
      | '>' =>
        switch (item.[1]) {
        | '=' =>
          Constraint.GTE(exactPartial(parsePartial(sliceToEnd(item, 2))))
        | _ =>
          Constraint.GT(exactPartial(parsePartial(sliceToEnd(item, 1))))
        }
      | '<' =>
        switch (item.[1]) {
        | '=' =>
          Constraint.LTE(exactPartial(parsePartial(sliceToEnd(item, 2))))
        | _ =>
          Constraint.LT(exactPartial(parsePartial(sliceToEnd(item, 1))))
        }
      | _ =>
        let msg = Printf.sprintf("bad version: %s", item);
        failwith(msg);
      };

    let parseSimple = item =>
      switch (item.[0]) {
      | '~' =>
        switch (parsePartial(sliceToEnd(item, 1))) {
        | `Major(m, prerelease, build) => [
            Constraint.GTE(Version.make(~prerelease, ~build, m, 0, 0)),
            Constraint.LT(Version.make(m + 1, 0, 0)),
          ]
        | `Minor(m, i, prerelease, build) => [
            Constraint.GTE(Version.make(~prerelease, ~build, m, i, 0)),
            Constraint.LT(Version.make(m, i + 1, 0)),
          ]
        | `Patch(m, i, p, prerelease, build) => [
            Constraint.GTE(Version.make(~prerelease, ~build, m, i, p)),
            Constraint.LT(Version.make(m, i + 1, 0)),
          ]
        | `AllStar => failwith("* cannot be tilded")
        | `MajorStar(m) => [
            Constraint.GTE(Version.make(m, 0, 0)),
            Constraint.LT(Version.make(m + 1, 0, 0)),
          ]
        | `MinorStar(m, i) => [
            Constraint.GTE(Version.make(m, i, 0)),
            Constraint.LT(Version.make(m, i + 1, 0)),
          ]
        | `Raw(_) => failwith("Bad tilde")
        }

      | '^' =>
        switch (parsePartial(sliceToEnd(item, 1))) {
        | `Major(m, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, m, 0, 0)),
            LT(Version.make(m + 1, 0, 0)),
          ]
        | `Minor(0, i, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, 0, i, 0)),
            LT(Version.make(0, i + 1, 0)),
          ]
        | `Minor(m, i, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, m, i, 0)),
            LT(Version.make(m + 1, 0, 0)),
          ]
        | `Patch(0, 0, p, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, 0, 0, p)),
            LT(Version.make(0, 0, p + 1)),
          ]
        | `Patch(0, i, p, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, 0, i, p)),
            LT(Version.make(0, i + 1, 0)),
          ]
        | `Patch(m, i, p, prerelease, build) => [
            GTE(Version.make(~prerelease, ~build, m, i, p)),
            LT(Version.make(m + 1, 0, 0)),
          ]
        | `AllStar => failwith("* cannot be careted")
        | `MajorStar(m) => [
            GTE(Version.make(m, 0, 0)),
            LT(Version.make(m + 1, 0, 0)),
          ]
        | `MinorStar(m, i) => [
            GTE(Version.make(m, i, 0)),
            LT(Version.make(m + 1, i, 0)),
          ]
        | `Raw(_) => failwith("Bad tilde")
        }

      | '>'
      | '<'
      | '=' => [parsePrimitive(item)]

      | _ =>
        switch (parsePartial(item)) {
        | `AllStar => [ANY]
        | `Major(m, [], [])
        | `MajorStar(m) => [
            GTE(Version.make(m, 0, 0)),
            LT(Version.make(m + 1, 0, 0)),
          ]
        | `Major(m, prerelease, build) => [
            EQ(Version.make(~prerelease, ~build, m, 0, 0)),
          ]
        | `Minor(m, i, [], [])
        | `MinorStar(m, i) => [
            GTE(Version.make(m, i, 0)),
            LT(Version.make(m, i + 1, 0)),
          ]
        | `Minor(m, i, prerelease, build) => [
            EQ(Version.make(~prerelease, ~build, m, i, 0)),
          ]
        | `Patch(m, i, p, prerelease, build) => [
            EQ(Version.make(~prerelease, ~build, m, i, p)),
          ]
        | `Raw(_prerelease, _build) => failwith("bad version")
        }
      };

    let parseConj = v => {
      let vs = Str.split(Str.regexp(" +"), v);
      let vs = {
        let f = (vs, v) => vs @ parseSimple(v);
        List.fold_left(f, [], vs);
      };

      vs;
    };

    let parseNpmRange = v => {
      let v =
        v
        |> Str.global_replace(Str.regexp(">= +"), ">=")
        |> Str.global_replace(Str.regexp("<= +"), "<=")
        |> Str.global_replace(Str.regexp("> +"), ">")
        |> Str.global_replace(Str.regexp("< +"), "<")
        |> Str.global_replace(Str.regexp("= +"), "=")
        |> Str.global_replace(Str.regexp("~ +"), "~")
        |> Str.global_replace(Str.regexp("^ +"), "^");

      let vs = Str.split(Str.regexp(" +- +"), v);
      switch (vs) {
      | [item] => parseConj(item)
      | [left, right] =>
        let left = Constraint.GTE(parsePartial(left) |> exactPartial);
        let right =
          switch (parsePartial(right)) {
          | `AllStar => Constraint.ANY
          | `Major(m, _, _)
          | `MajorStar(m) => Constraint.LT(Version.make(m + 1, 0, 0))
          | `Minor(m, i, _, _)
          | `MinorStar(m, i) => Constraint.LT(Version.make(m, i + 1, 0))
          | `Patch(m, i, p, prerelease, build) =>
            Constraint.LTE(Version.make(~prerelease, ~build, m, i, p))
          | `Raw(prerelease, build) =>
            Constraint.LT(Version.make(~prerelease, ~build, 0, 0, 0))
          };

        [left, right];
      | _ =>
        let msg = Printf.sprintf("invalid version: %s", v);
        failwith(msg);
      };
    };

    let parse = ParseUtils.disjunction(~parse=parseNpmRange);
  };

  let parse = formula =>
    try(Ok(Parser.parse(formula))) {
    | Failure(message) =>
      Error("Failed with message: " ++ message ++ " : " ++ formula)
    | e =>
      Error(
        "Invalid formula (pretending its any): "
        ++ formula
        ++ " "
        ++ Printexc.to_string(e),
      )
    };

  let parseExn = formula =>
    switch (parse(formula)) {
    | Ok(f) => f
    | Error(err) => raise(Invalid_argument(err))
    };

  let parserDnf = {
    let p = parse;
    open Angstrom;
    let* input = take_while1(_ => true);
    switch (p(input)) {
    | Ok(v) => return(v)
    | Error(msg) => fail(msg)
    };
  };
};

let caretRangeOfVersion = (version: Version.t) => {
  let upperBound =
    if (version.major < 1) {
      Version.{
        major: 0,
        minor: version.minor + 1,
        patch: 0,
        prerelease: [],
        build: [],
      };
    } else {
      Version.{
        major: version.major + 1,
        minor: 0,
        patch: 0,
        prerelease: [],
        build: [],
      };
    };

  [[Constraint.GTE(version), Constraint.LT(upperBound)]];
};
