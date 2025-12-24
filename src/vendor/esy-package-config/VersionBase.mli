(** This module defines utilities for working with versions.*)

module type VERSION = sig
  type t

  val compare : t -> t -> int
  val parser : t Angstrom.t
  val parse : string -> (t, string) result
  val parseExn : string -> t
  val majorMinorPatch : t -> (int * int * int) option
  val prerelease : t -> bool
  val stripPrerelease : t -> t
end
[@@ocaml.doc
  "\n\
  \ * Type for versions.\n\
  \ *\n\
  \ * opam's versions and npm's semver versions implement this.\n\
  \ "]

module type CONSTRAINT = sig
  type version

  type t =
    | EQ of version
    | NEQ of version
    | GT of version
    | GTE of version
    | LT of version
    | LTE of version
    | ANY

  val compare : t -> t -> int

  module VersionSet : Set.S with type elt = version

  val matchesSimple : version:version -> t -> bool
  val matches : ?matchPrerelease:VersionSet.t -> version:version -> t -> bool
  val map : f:(version -> version) -> t -> t
end
[@@ocaml.doc "\n * Constraints over versions.\n "]

module type FORMULA = sig
  type version
  type constr
  type 'f conj = 'f list
  type 'f disj = 'f list

  module DNF : sig
    type t = constr conj disj

    val compare : t -> t -> int
    val unit : constr -> t
    val matches : version:version -> t -> bool
    val map : f:(version -> version) -> t -> t
    val conj : t -> t -> t
    val disj : constr disj -> constr disj -> constr disj
  end
  [@@ocaml.doc "\n   * Disjnuction normal form.\n   "]

  module CNF : sig
    type t = constr disj conj

    val compare : t -> t -> int
    val matches : version:version -> constr disj disj -> bool
  end
  [@@ocaml.doc "\n   * Conjunction normal form.\n   "]

  val ofDnfToCnf : DNF.t -> CNF.t
  [@@ocaml.doc "\n   * Convert from DNF to CNF.\n   "]

  module ParseUtils : sig
    val conjunction : parse:(string -> 'a) -> string -> 'a disj

    val disjunction :
      parse:(string -> constr disj) -> string -> constr disj disj
  end
end
[@@ocaml.doc "\n * Formulas over constraints.\n "]

module Constraint : sig
  module Make : (Version : VERSION) -> CONSTRAINT with type version = Version.t
end

module Formula : sig
  module Make : (Version : VERSION)
    (Constraint : CONSTRAINT with type version = Version.t)
    -> FORMULA with type version = Version.t and type constr = Constraint.t
end
