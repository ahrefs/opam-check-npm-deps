type t =
  | Npm of SemverVersion.Version.t
  | Opam of OpamPackageVersion.Version.t
  | Source of Source.t
[@@deriving ord]

let show v =
  match v with
  | Npm t -> SemverVersion.Version.show t
  | Opam v -> "opam:" ^ OpamPackageVersion.Version.show v
  | Source src -> Source.show src

let showSimple v =
  match v with
  | Npm t -> SemverVersion.Version.show t
  | Opam v -> OpamPackageVersion.Version.show v
  | Source src -> Source.show src

let pp fmt v = Fmt.fmt "%s" fmt (show v)

module Parse = struct
  include Parse

  let npm = SemverVersion.Version.parser >>| fun v -> Npm v
  let opam = OpamPackageVersion.Version.parser >>| fun v -> Opam v
  let opamWithPrefix = string "opam:" *> commit >> opam
  let sourceRelaxed = Source.parserRelaxed >>| fun source -> Source source
end

let parse ?(tryAsOpam = false) v =
  let parser =
    if tryAsOpam then
      let open Parse in
      opamWithPrefix <|> opam <|> sourceRelaxed
    else
      let open Parse in
      opamWithPrefix <|> npm <|> sourceRelaxed
  in
  Parse.parse parser v

let parseExn v = match parse v with Ok v -> v | Error err -> failwith err
let to_yojson v = `String (show v)

let of_yojson json =
  let open Result.Syntax in
  let* v = Json.Decode.string json in
  parse v

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
