module Version = struct
  type t = {
    major : int;
    minor : int;
    patch : int;
    prerelease : prerelease;
    build : build;
  }

  and prerelease = segment list
  and build = string list
  and segment = W of string | N of int

  let compareSegment a =
   fun b ->
    match (a, b) with
    | N _, W _ -> -1
    | W _, N _ -> 1
    | N a, N b -> compare a b
    | W a, W b -> String.compare a b

  let make ?(prerelease = []) =
   fun ?(build = []) ->
    fun major ->
     fun minor -> fun patch -> { major; minor; patch; prerelease; build }

  let majorMinorPatch v = Some (v.major, v.minor, v.patch)

  let prerelease v =
    match (v.prerelease, v.build) with [], [] -> false | _, _ -> true

  let stripPrerelease v = { v with prerelease = []; build = [] }

  module Parse = struct
    open Re

    let dot = char '.'
    let dash = char '-'
    let plus = char '+'
    let section = group (rep1 digit)
    let prereleaseChar = alt [ alnum; char '-'; char '.' ]
    let prerelease = opt (seq [ opt dash; group (rep1 prereleaseChar) ])
    let build = opt (seq [ opt plus; group (rep1 prereleaseChar) ])
    let prefix = rep (alt [ char 'v'; char '=' ])

    let version3 =
      compile
        (seq
           [
             bos;
             prefix;
             section;
             dot;
             section;
             dot;
             section;
             prerelease;
             build;
             eos;
           ])

    let version2 =
      compile
        (seq [ bos; prefix; section; dot; section; prerelease; build; eos ])

    let version1 =
      compile (seq [ bos; prefix; section; prerelease; build; eos ])

    let prerelaseAndBuild = compile (seq [ bos; prerelease; build; eos ])
  end

  let intAtExn n =
   fun m ->
    let v = Re.Group.get m n in
    int_of_string v

  let optStrignAt n =
   fun m ->
    match Re.Group.get m n with
    | exception Not_found -> None
    | "" -> None
    | v -> Some v

  let parsePrerelease v =
    v |> String.split_on_char '.'
    |> List.map (fun v -> try N (int_of_string v) with _ -> W v)

  let parseBuild v = String.split_on_char '.' v

  let parsePrerelaseAndBuild v =
    match Re.exec_opt Parse.prerelaseAndBuild v with
    | Some m ->
        let prerelease =
          match optStrignAt 1 m with Some v -> parsePrerelease v | None -> []
        in
        let build =
          match optStrignAt 2 m with Some v -> parseBuild v | None -> []
        in
        (Ok (prerelease, build) [@implicit_arity])
    | None ->
        let msg = Printf.sprintf "unable to parse prerelease part: %s" v in
        Error msg

  let parse version =
    match Re.exec_opt Parse.version3 version with
    | Some m ->
        let major = intAtExn 1 m in
        let minor = intAtExn 2 m in
        let patch = intAtExn 3 m in
        let prerelease =
          match optStrignAt 4 m with Some v -> parsePrerelease v | None -> []
        in
        let build =
          match optStrignAt 5 m with Some v -> parseBuild v | None -> []
        in
        Ok { major; minor; patch; prerelease; build }
    | None -> (
        match Re.exec_opt Parse.version2 version with
        | Some m ->
            let major = intAtExn 1 m in
            let minor = intAtExn 2 m in
            let prerelease =
              match optStrignAt 3 m with
              | Some v -> parsePrerelease v
              | None -> []
            in
            let build =
              match optStrignAt 4 m with Some v -> parseBuild v | None -> []
            in
            Ok { major; minor; patch = 0; prerelease; build }
        | None -> (
            match Re.exec_opt Parse.version1 version with
            | Some m ->
                let major = intAtExn 1 m in
                let prerelease =
                  match optStrignAt 2 m with
                  | Some v -> parsePrerelease v
                  | None -> []
                in
                let build =
                  match optStrignAt 3 m with
                  | Some v -> parseBuild v
                  | None -> []
                in
                Ok { major; minor = 0; patch = 0; prerelease; build }
            | None ->
                let msg =
                  Printf.sprintf "invalid semver version: '%s'" version
                in
                Error msg))

  let parseExn v =
    match parse v with Ok v -> v | Error err -> raise (Invalid_argument err)

  let parser =
    let p = parse in
    let open Angstrom in
    let* input = take_while1 (fun _ -> true) in
    match p input with Ok v -> return v | Error msg -> fail msg

  let comparePrerelease (a : segment list) =
   fun (b : segment list) ->
    let rec compare a =
     fun b ->
      match (a, b) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs, y :: ys -> (
          match compareSegment x y with 0 -> compare xs ys | v -> v)
    in
    match (a, b) with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | a, b -> compare a b

  let compareBuild (a : string list) =
   fun (b : string list) ->
    let rec compare a =
     fun b ->
      match (a, b) with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs, y :: ys -> (
          match String.compare x y with 0 -> compare xs ys | v -> v)
    in
    match (a, b) with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1
    | a, b -> compare a b

  let compare a =
   fun b ->
    match a.major - b.major with
    | 0 -> (
        match a.minor - b.minor with
        | 0 -> (
            match a.patch - b.patch with
            | 0 -> (
                match comparePrerelease a.prerelease b.prerelease with
                | 0 -> compareBuild a.build b.build
                | v -> v)
            | v -> v)
        | v -> v)
    | v -> v
end

module Constraint = VersionBase.Constraint.Make (Version)

module Formula = struct
  include VersionBase.Formula.Make (Version) (Constraint)

  let any : DNF.t = ([ [ Constraint.ANY ] ] : DNF.t)

  module Parser = struct
    let sliceToEnd text =
     fun num -> String.sub text num (String.length text - num)

    let isint v =
      try
        ignore (int_of_string v);
        true
      with _ -> false

    let parsePrerelaseAndBuild v =
      match Version.parsePrerelaseAndBuild v with
      | Ok v -> v
      | Error err -> failwith err

    let exactPartial partial =
      match partial with
      | `AllStar -> failwith "* cannot be compared"
      | `MajorStar major -> Version.make major 0 0
      | `MinorStar (major, minor) -> Version.make major minor 0
      | `Major (major, prerelease, build) ->
          Version.make ~prerelease ~build major 0 0
      | `Minor (major, minor, prerelease, build) ->
          Version.make ~prerelease ~build major minor 0
      | `Patch (major, minor, patch, prerelease, build) ->
          Version.make ~prerelease ~build major minor patch
      | `Raw (prerelease, build) -> Version.make ~prerelease ~build 0 0 0

    let parsePartial version =
      let version =
        match version.[0] = '=' with
        | true -> sliceToEnd version 1
        | false -> version
      in
      let version =
        match version.[0] = 'v' with
        | true -> sliceToEnd version 1
        | false -> version
      in
      let parts = String.split_on_char '.' version in
      match parts with
      | ("*" | "x" | "X") :: _rest -> `AllStar
      | major :: ("*" | "x" | "X") :: _rest when isint major ->
          `MajorStar (int_of_string major)
      | major :: minor :: ("*" | "x" | "X") :: _rest
        when isint major && isint minor ->
          `MinorStar (int_of_string major, int_of_string minor)
      | _ -> (
          let rx =
            Str.regexp
              {|^ *\([0-9]+\)\(\.\([0-9]+\)\(\.\([0-9]+\)\)?\)?\(\([-+~][a-z0-9\.]+\)\)?|}
          in
          match Str.search_forward rx version 0 with
          | exception Not_found -> `Raw (parsePrerelaseAndBuild version)
          | _ -> (
              let major = int_of_string (Str.matched_group 1 version) in
              let prerelease, build =
                match Str.matched_group 7 version with
                | exception Not_found ->
                    let last = Str.match_end () in
                    if last < String.length version then
                      parsePrerelaseAndBuild (sliceToEnd version last)
                    else ([], [])
                | text -> parsePrerelaseAndBuild text
              in
              match Str.matched_group 3 version with
              | exception Not_found -> `Major (major, prerelease, build)
              | minor -> (
                  let minor = int_of_string minor in
                  match Str.matched_group 5 version with
                  | exception Not_found ->
                      `Minor (major, minor, prerelease, build)
                  | patch ->
                      `Patch
                        (major, minor, int_of_string patch, prerelease, build)))
          )

    let parsePrimitive item =
      match item.[0] with
      | '=' -> Constraint.EQ (exactPartial (parsePartial (sliceToEnd item 1)))
      | '>' -> (
          match item.[1] with
          | '=' ->
              Constraint.GTE (exactPartial (parsePartial (sliceToEnd item 2)))
          | _ -> Constraint.GT (exactPartial (parsePartial (sliceToEnd item 1)))
          )
      | '<' -> (
          match item.[1] with
          | '=' ->
              Constraint.LTE (exactPartial (parsePartial (sliceToEnd item 2)))
          | _ -> Constraint.LT (exactPartial (parsePartial (sliceToEnd item 1)))
          )
      | _ ->
          let msg = Printf.sprintf "bad version: %s" item in
          failwith msg

    let parseSimple item =
      match item.[0] with
      | '~' -> (
          match parsePartial (sliceToEnd item 1) with
          | `Major (m, prerelease, build) ->
              [
                Constraint.GTE (Version.make ~prerelease ~build m 0 0);
                Constraint.LT (Version.make (m + 1) 0 0);
              ]
          | `Minor (m, i, prerelease, build) ->
              [
                Constraint.GTE (Version.make ~prerelease ~build m i 0);
                Constraint.LT (Version.make m (i + 1) 0);
              ]
          | `Patch (m, i, p, prerelease, build) ->
              [
                Constraint.GTE (Version.make ~prerelease ~build m i p);
                Constraint.LT (Version.make m (i + 1) 0);
              ]
          | `AllStar -> failwith "* cannot be tilded"
          | `MajorStar m ->
              [
                Constraint.GTE (Version.make m 0 0);
                Constraint.LT (Version.make (m + 1) 0 0);
              ]
          | `MinorStar (m, i) ->
              [
                Constraint.GTE (Version.make m i 0);
                Constraint.LT (Version.make m (i + 1) 0);
              ]
          | `Raw _ -> failwith "Bad tilde")
      | '^' -> (
          match parsePartial (sliceToEnd item 1) with
          | `Major (m, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build m 0 0);
                LT (Version.make (m + 1) 0 0);
              ]
          | `Minor (0, i, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build 0 i 0);
                LT (Version.make 0 (i + 1) 0);
              ]
          | `Minor (m, i, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build m i 0);
                LT (Version.make (m + 1) 0 0);
              ]
          | `Patch (0, 0, p, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build 0 0 p);
                LT (Version.make 0 0 (p + 1));
              ]
          | `Patch (0, i, p, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build 0 i p);
                LT (Version.make 0 (i + 1) 0);
              ]
          | `Patch (m, i, p, prerelease, build) ->
              [
                GTE (Version.make ~prerelease ~build m i p);
                LT (Version.make (m + 1) 0 0);
              ]
          | `AllStar -> failwith "* cannot be careted"
          | `MajorStar m ->
              [ GTE (Version.make m 0 0); LT (Version.make (m + 1) 0 0) ]
          | `MinorStar (m, i) ->
              [ GTE (Version.make m i 0); LT (Version.make (m + 1) i 0) ]
          | `Raw _ -> failwith "Bad tilde")
      | '>' | '<' | '=' -> [ parsePrimitive item ]
      | _ -> (
          match parsePartial item with
          | `AllStar -> [ ANY ]
          | `Major (m, [], []) | `MajorStar m ->
              [ GTE (Version.make m 0 0); LT (Version.make (m + 1) 0 0) ]
          | `Major (m, prerelease, build) ->
              [ EQ (Version.make ~prerelease ~build m 0 0) ]
          | `Minor (m, i, [], []) | `MinorStar (m, i) ->
              [ GTE (Version.make m i 0); LT (Version.make m (i + 1) 0) ]
          | `Minor (m, i, prerelease, build) ->
              [ EQ (Version.make ~prerelease ~build m i 0) ]
          | `Patch (m, i, p, prerelease, build) ->
              [ EQ (Version.make ~prerelease ~build m i p) ]
          | `Raw (_prerelease, _build) -> failwith "bad version")

    let parseConj v =
      let vs = Str.split (Str.regexp " +") v in
      let vs =
        let f vs = fun v -> vs @ parseSimple v in
        List.fold_left f [] vs
      in
      vs

    let parseNpmRange v =
      let v =
        v
        |> Str.global_replace (Str.regexp ">= +") ">="
        |> Str.global_replace (Str.regexp "<= +") "<="
        |> Str.global_replace (Str.regexp "> +") ">"
        |> Str.global_replace (Str.regexp "< +") "<"
        |> Str.global_replace (Str.regexp "= +") "="
        |> Str.global_replace (Str.regexp "~ +") "~"
        |> Str.global_replace (Str.regexp "^ +") "^"
      in
      let vs = Str.split (Str.regexp " +- +") v in
      match vs with
      | item :: [] -> parseConj item
      | [ left; right ] ->
          let left = Constraint.GTE (parsePartial left |> exactPartial) in
          let right =
            match parsePartial right with
            | `AllStar -> Constraint.ANY
            | `Major (m, _, _) | `MajorStar m ->
                Constraint.LT (Version.make (m + 1) 0 0)
            | `Minor (m, i, _, _) | `MinorStar (m, i) ->
                Constraint.LT (Version.make m (i + 1) 0)
            | `Patch (m, i, p, prerelease, build) ->
                Constraint.LTE (Version.make ~prerelease ~build m i p)
            | `Raw (prerelease, build) ->
                Constraint.LT (Version.make ~prerelease ~build 0 0 0)
          in
          [ left; right ]
      | _ ->
          let msg = Printf.sprintf "invalid version: %s" v in
          failwith msg

    let parse = ParseUtils.disjunction ~parse:parseNpmRange
  end

  let parse formula =
    try Ok (Parser.parse formula) with
    | Failure message ->
        Error ("Failed with message: " ^ message ^ " : " ^ formula)
    | e ->
        Error
          ("Invalid formula (pretending its any): " ^ formula ^ " "
         ^ Printexc.to_string e)

  let parseExn formula =
    match parse formula with
    | Ok f -> f
    | Error err -> raise (Invalid_argument err)

  let parserDnf =
    let p = parse in
    let open Angstrom in
    let* input = take_while1 (fun _ -> true) in
    match p input with Ok v -> return v | Error msg -> fail msg
end

let caretRangeOfVersion (version : Version.t) =
  let upperBound =
    if version.major < 1 then
      let open Version in
      {
        major = 0;
        minor = version.minor + 1;
        patch = 0;
        prerelease = [];
        build = [];
      }
    else
      let open Version in
      {
        major = version.major + 1;
        minor = 0;
        patch = 0;
        prerelease = [];
        build = [];
      }
  in
  [ [ Constraint.GTE version; Constraint.LT upperBound ] ]
