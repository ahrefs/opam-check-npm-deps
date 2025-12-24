module List = ListLabels

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

  module VersionSet : Set.S with type elt = version

  val matchesSimple : version:version -> t -> bool
  val matches : ?matchPrerelease:VersionSet.t -> version:version -> t -> bool
  val map : f:(version -> version) -> t -> t
end

module type FORMULA = sig
  type version
  type constr
  type 'f conj = 'f list
  type 'f disj = 'f list

  module DNF : sig
    type t = constr conj disj

    val unit : constr -> t
    val matches : version:version -> t -> bool
    val map : f:(version -> version) -> t -> t
    val conj : t -> t -> t
    val disj : constr disj -> constr disj -> constr disj
  end

  module CNF : sig
    type t = constr disj conj

    val matches : version:version -> t -> bool
  end

  val ofDnfToCnf : DNF.t -> CNF.t

  module ParseUtils : sig
    val conjunction : parse:(string -> 'a) -> string -> 'a disj

    val disjunction :
      parse:(string -> constr disj) -> string -> constr disj disj
  end
end

module Constraint = struct
  module Make (Version : VERSION) : CONSTRAINT with type version = Version.t =
  struct
    module VersionSet = Set.Make (Version)

    type version = Version.t

    type t =
      | EQ of Version.t
      | NEQ of Version.t
      | GT of Version.t
      | GTE of Version.t
      | LT of Version.t
      | LTE of Version.t
      | ANY

    let matchesSimple ~version =
     fun constr ->
      match constr with
      | EQ a -> Version.compare a version = 0
      | NEQ a -> Version.compare a version != 0
      | ANY -> true
      | GT a -> Version.compare a version < 0
      | GTE a -> Version.compare a version <= 0
      | LT a -> Version.compare a version > 0
      | LTE a -> Version.compare a version >= 0

    let matches ?(matchPrerelease = VersionSet.empty) =
     fun ~version ->
      fun constr ->
       match (Version.prerelease version, constr) with
       | _, EQ _
       | _, NEQ _
       | false, ANY
       | false, GT _
       | false, GTE _
       | false, LT _
       | false, LTE _ ->
           matchesSimple ~version constr
       | true, ANY | true, GT _ | true, GTE _ | true, LT _ | true, LTE _ ->
           if VersionSet.mem (Version.stripPrerelease version) matchPrerelease
           then matchesSimple ~version constr
           else false

    let map ~f =
     fun constr ->
      match constr with
      | EQ a -> EQ (f a)
      | NEQ a -> NEQ (f a)
      | ANY -> ANY
      | GT a -> GT (f a)
      | GTE a -> GTE (f a)
      | LT a -> LT (f a)
      | LTE a -> LTE (f a)
  end
end
[@@ocaml.doc " Constraints over versions "]

module Formula = struct
  module Make
      (Version : VERSION)
      (Constraint : CONSTRAINT with type version = Version.t) :
    FORMULA
      with type version = Constraint.version
       and type constr = Constraint.t = struct
    type version = Constraint.version
    type constr = Constraint.t
    type 'f conj = 'f list
    type 'f disj = 'f list

    module VersionSet = Constraint.VersionSet

    module DNF = struct
      type t = Constraint.t conj disj

      let unit constr = [ [ constr ] ]

      let matches ~version =
       fun formulas ->
        let matchesConj formulas =
          let matchPrerelease =
            let f vs =
             (function
             | Constraint.ANY -> vs
             | Constraint.EQ v
             | Constraint.NEQ v
             | Constraint.LTE v
             | Constraint.LT v
             | Constraint.GTE v
             | Constraint.GT v ->
                 if Version.prerelease v then
                   VersionSet.add (Version.stripPrerelease v) vs
                 else vs)
            in
            List.fold_left ~f ~init:VersionSet.empty formulas
          in
          List.for_all
            ~f:(Constraint.matches ~matchPrerelease ~version)
            formulas
        in
        List.exists ~f:matchesConj formulas

      let map ~f =
       fun formulas ->
        let mapConj formulas = List.map ~f:(Constraint.map ~f) formulas in
        List.map ~f:mapConj formulas

      let conj a =
       fun b ->
        let items =
          let items = [] in
          let f items =
           fun a ->
            let f items = fun b -> (a @ b) :: items in
            List.fold_left ~f ~init:items b
          in
          List.fold_left ~f ~init:items a
        in
        items

      let disj a = fun b -> a @ b
    end

    module CNF = struct
      type t = Constraint.t disj conj

      let matches ~version =
       fun formulas ->
        let matchesDisj formulas =
          let matchPrerelease =
            let f vs =
             (function
             | Constraint.ANY -> vs
             | Constraint.EQ v
             | Constraint.NEQ v
             | Constraint.LTE v
             | Constraint.LT v
             | Constraint.GTE v
             | Constraint.GT v ->
                 if Version.prerelease v then
                   VersionSet.add (Version.stripPrerelease v) vs
                 else vs)
            in
            List.fold_left ~f ~init:VersionSet.empty formulas
          in
          List.exists ~f:(Constraint.matches ~matchPrerelease ~version) formulas
        in
        List.for_all ~f:matchesDisj formulas
    end

    let ofDnfToCnf (f : DNF.t) =
      let f : CNF.t =
        (match f with
         | [] -> []
         | constrs :: conjs ->
             let init : constr disj list =
               (List.map ~f:(fun r -> [ r ]) constrs : constr disj list)
             in
             let conjs =
               let addConj (cnf : constr disj list) =
                fun conj ->
                 cnf
                 |> List.map ~f:(fun constrs ->
                     List.map ~f:(fun r -> r :: constrs) conj)
                 |> List.flatten
               in
               List.fold_left ~f:addConj ~init conjs
             in
             conjs
          : CNF.t)
      in
      f

    module ParseUtils = struct
      let conjunction ~parse =
       fun item ->
        let item =
          item
          |> Str.global_replace (Str.regexp ">= +") ">="
          |> Str.global_replace (Str.regexp "<= +") "<="
          |> Str.global_replace (Str.regexp "> +") ">"
          |> Str.global_replace (Str.regexp "< +") "<"
        in
        let items = String.split_on_char ' ' item in
        List.map ~f:parse items

      let disjunction ~parse =
       fun version ->
        let version = String.trim version in
        let items = Str.split (Str.regexp " +|| +") version in
        let items = List.map ~f:parse items in
        let items =
          match items with [] -> [ [ Constraint.ANY ] ] | items -> items
        in
        items
    end
  end
end
