module List = ListLabels

module type VERSION = {
  type t;

  let compare: (t, t) => int;

  let parser: Angstrom.t(t);
  let parse: string => result(t, string);
  let parseExn: string => t;

  let majorMinorPatch: t => option((int, int, int));
  let prerelease: t => bool;
  let stripPrerelease: t => t;
};

module type CONSTRAINT = {
  type version;

  type t =
    | EQ(version)
    | NEQ(version)
    | GT(version)
    | GTE(version)
    | LT(version)
    | LTE(version)
    | ANY;

  let compare: (t, t) => int;

  module VersionSet: Set.S with type elt = version;

  let matchesSimple: (~version: version, t) => bool;

  let matches:
    (~matchPrerelease: VersionSet.t=?, ~version: version, t) => bool;

  let map: (~f: version => version, t) => t;
};

module type FORMULA = {
  type version;
  type constr;

  type conj('f) = list('f);
  type disj('f) = list('f);

  module DNF: {
    type t = disj(conj(constr));

    let compare: (t, t) => int;

    let unit: constr => t;
    let matches: (~version: version, t) => bool;
    let map: (~f: version => version, t) => t;

    let conj: (t, t) => t;
    let disj: (disj(constr), disj(constr)) => disj(constr);
  };

  module CNF: {
    type t = conj(disj(constr));

    let compare: (t, t) => int;

    let matches: (~version: version, t) => bool;
  };

  let ofDnfToCnf: DNF.t => CNF.t;

  module ParseUtils: {
    let conjunction: (~parse: string => 'a, string) => disj('a);
    let disjunction:
      (~parse: string => disj(constr), string) => disj(disj(constr));
  };
};

/** Constraints over versions */
module Constraint = {
  module Make =
         (Version: VERSION)
         : (CONSTRAINT with type version = Version.t) => {
    module VersionSet = Set.Make(Version);

    type version = Version.t;

    [@deriving ord]
    type t =
      | EQ(Version.t)
      | NEQ(Version.t)
      | GT(Version.t)
      | GTE(Version.t)
      | LT(Version.t)
      | LTE(Version.t)
      | ANY;

    let matchesSimple = (~version, constr) =>
      switch (constr) {
      | EQ(a) => Version.compare(a, version) == 0
      | NEQ(a) => Version.compare(a, version) !== 0
      | ANY => true

      | GT(a) => Version.compare(a, version) < 0
      | GTE(a) => Version.compare(a, version) <= 0
      | LT(a) => Version.compare(a, version) > 0
      | LTE(a) => Version.compare(a, version) >= 0
      };

    let matches = (~matchPrerelease=VersionSet.empty, ~version, constr) =>
      switch (Version.prerelease(version), constr) {
      | (_, EQ(_))
      | (_, NEQ(_))
      | (false, ANY)
      | (false, GT(_))
      | (false, GTE(_))
      | (false, LT(_))
      | (false, LTE(_)) => matchesSimple(~version, constr)

      | (true, ANY)
      | (true, GT(_))
      | (true, GTE(_))
      | (true, LT(_))
      | (true, LTE(_)) =>
        if (VersionSet.mem(Version.stripPrerelease(version), matchPrerelease)) {
          matchesSimple(~version, constr);
        } else {
          false;
        }
      };

    let rec map = (~f, constr) =>
      switch (constr) {
      | EQ(a) => EQ(f(a))
      | NEQ(a) => NEQ(f(a))
      | ANY => ANY
      | GT(a) => GT(f(a))
      | GTE(a) => GTE(f(a))
      | LT(a) => LT(f(a))
      | LTE(a) => LTE(f(a))
      };
  };
};

module Formula = {
  module Make =
         (
           Version: VERSION,
           Constraint: CONSTRAINT with type version = Version.t,
         )

           : (
             FORMULA with
               type version = Constraint.version and type constr = Constraint.t
         ) => {
    type version = Constraint.version;
    type constr = Constraint.t;

    [@deriving (show, ord)]
    type conj('f) = list('f);

    [@deriving (show, ord)]
    type disj('f) = list('f);

    module VersionSet = Constraint.VersionSet;

    module DNF = {
      [@deriving ord]
      type t = disj(conj(Constraint.t));

      let unit = constr => [[constr]];

      let matches = (~version, formulas) => {
        let matchesConj = formulas => {
          /* Within each conjunction we allow prelease versions to be matched
           * but only those were mentioned in any of the constraints of the
           * conjunction, so that:
           *  1.0.0-alpha.2 matches >=1.0.0.alpha1
           *  1.0.0-alpha.2 does not match >=0.9.0
           *  1.0.0-alpha.2 does not match >=0.9.0 <2.0.0
           */
          let matchPrerelease = {
            let f = vs =>
              fun
              | Constraint.ANY => vs
              | Constraint.EQ(v)
              | Constraint.NEQ(v)
              | Constraint.LTE(v)
              | Constraint.LT(v)
              | Constraint.GTE(v)
              | Constraint.GT(v) =>
                if (Version.prerelease(v)) {
                  VersionSet.add(Version.stripPrerelease(v), vs);
                } else {
                  vs;
                };

            List.fold_left(~f, ~init=VersionSet.empty, formulas);
          };

          List.for_all(
            ~f=Constraint.matches(~matchPrerelease, ~version),
            formulas,
          );
        };

        List.exists(~f=matchesConj, formulas);
      };

      let rec map = (~f, formulas) => {
        let mapConj = formulas => List.map(~f=Constraint.map(~f), formulas);
        List.map(~f=mapConj, formulas);
      };

      let conj = (a, b) => {
        let items = {
          let items = [];
          let f = (items, a) => {
            let f = (items, b) => [a @ b, ...items];

            List.fold_left(~f, ~init=items, b);
          };

          List.fold_left(~f, ~init=items, a);
        };
        items;
      };

      let disj = (a, b) => a @ b;
    };

    module CNF = {
      [@deriving ord]
      type t = conj(disj(Constraint.t));

      let matches = (~version, formulas) => {
        let matchesDisj = formulas => {
          /* Within each conjunction we allow prelease versions to be matched
           * but only those were mentioned in any of the constraints of the
           * conjunction, so that:
           *  1.0.0-alpha.2 matches >=1.0.0.alpha1
           *  1.0.0-alpha.2 does not match >=0.9.0
           *  1.0.0-alpha.2 does not match >=0.9.0 <2.0.0
           */
          let matchPrerelease = {
            let f = vs =>
              fun
              | Constraint.ANY => vs
              | Constraint.EQ(v)
              | Constraint.NEQ(v)
              | Constraint.LTE(v)
              | Constraint.LT(v)
              | Constraint.GTE(v)
              | Constraint.GT(v) =>
                if (Version.prerelease(v)) {
                  VersionSet.add(Version.stripPrerelease(v), vs);
                } else {
                  vs;
                };

            List.fold_left(~f, ~init=VersionSet.empty, formulas);
          };

          List.exists(
            ~f=Constraint.matches(~matchPrerelease, ~version),
            formulas,
          );
        };

        List.for_all(~f=matchesDisj, formulas);
      };
    };

    let ofDnfToCnf = (f: DNF.t) => {
      let f: CNF.t = (
        switch (f) {
        | [] => []
        | [constrs, ...conjs] =>
          let init: list(disj(constr)) = (
            List.map(~f=r => [r], constrs): list(disj(constr))
          );
          let conjs = {
            let addConj = (cnf: list(disj(constr)), conj) =>
              cnf
              |> List.map(~f=constrs =>
                   List.map(~f=r => [r, ...constrs], conj)
                 )
              |> List.flatten;

            List.fold_left(~f=addConj, ~init, conjs);
          };

          conjs;
        }: CNF.t
      );
      f;
    };

    module ParseUtils = {
      let conjunction = (~parse, item) => {
        let item =
          item
          |> Str.global_replace(Str.regexp(">= +"), ">=")
          |> Str.global_replace(Str.regexp("<= +"), "<=")
          |> Str.global_replace(Str.regexp("> +"), ">")
          |> Str.global_replace(Str.regexp("< +"), "<");

        let items = String.split_on_char(' ', item);
        List.map(~f=parse, items);
      };

      let disjunction = (~parse, version) => {
        let version = String.trim(version);
        let items = Str.split(Str.regexp(" +|| +"), version);
        let items = List.map(~f=parse, items);
        let items =
          switch (items) {
          | [] => [[Constraint.ANY]]
          | items => items
          };

        items;
      };
    };
  };
};

