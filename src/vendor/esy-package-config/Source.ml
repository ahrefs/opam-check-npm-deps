type t =
  | Dist of Dist.t
  | Link of {
      path : DistPath.t;
      manifest : ManifestSpec.t option;
      kind : linkKind;
    }
[@@deriving ord]

and linkKind = LinkRegular | LinkDev

let manifest (src : t) =
  match src with Link info -> info.manifest | Dist dist -> Dist.manifest dist

let toDist (src : t) =
  match src with
  | Dist dist -> dist
  | Link { path; manifest; kind = _ } -> Dist.LocalPath { path; manifest }

let show' ~showPath = function
  | Dist (Github { user; repo; commit; manifest = None }) ->
      Printf.sprintf "github:%s/%s#%s" user repo commit
  | Dist (Github { user; repo; commit; manifest = Some manifest }) ->
      Printf.sprintf "github:%s/%s:%s#%s" user repo
        (ManifestSpec.show manifest)
        commit
  | Dist (Git { remote; commit; manifest = None }) ->
      Printf.sprintf "git:%s#%s" remote commit
  | Dist (Git { remote; commit; manifest = Some manifest }) ->
      Printf.sprintf "git:%s:%s#%s" remote (ManifestSpec.show manifest) commit
  | Dist (Archive { url; checksum }) ->
      Printf.sprintf "archive:%s#%s" url (Checksum.show checksum)
  | Dist (LocalPath { path; manifest = None }) ->
      Printf.sprintf "path:%s" (showPath path)
  | Dist (LocalPath { path; manifest = Some manifest }) ->
      Printf.sprintf "path:%s/%s" (showPath path) (ManifestSpec.show manifest)
  | Dist NoSource -> "no-source:"
  | Link { path; manifest = None; kind = LinkRegular } ->
      Printf.sprintf "link:%s" (showPath path)
  | Link { path; manifest = Some manifest; kind = LinkRegular } ->
      Printf.sprintf "link:%s/%s" (showPath path) (ManifestSpec.show manifest)
  | Link { path; manifest = None; kind = LinkDev } ->
      Printf.sprintf "link-dev:%s" (showPath path)
  | Link { path; manifest = Some manifest; kind = LinkDev } ->
      Printf.sprintf "link-dev:%s/%s" (showPath path)
        (ManifestSpec.show manifest)

let show = show' ~showPath:DistPath.show
let showPretty = show' ~showPath:DistPath.showPretty
let pp fmt src = Fmt.pf fmt "%s" (show src)
let ppPretty fmt src = Fmt.pf fmt "%s" (showPretty src)

module Parse = struct
  include Parse

  let withPrefix prefix p = string prefix *> p

  let pathLike ~requirePathSep make =
    let make path =
      let path =
        let open Path in
        normalizeAndRemoveEmptySeg (v path)
      in
      let path, manifest =
        match
          (Fs.isDirSync path, ManifestSpec.ofString (Path.basename path))
        with
        | true, _ -> (path, None)
        | false, Ok manifest ->
            let path =
              let open Path in
              remEmptySeg (parent path)
            in
            (path, Some manifest)
        | false, Error _ -> (path, None)
      in
      make (DistPath.ofPath path) manifest
    in
    let path =
      scan false (fun seenPathSep c -> Some (seenPathSep || c = '/'))
    in
    let* path, seenPathSep = path in
    if (not requirePathSep) || seenPathSep then return (make path)
    else fail "not a path"

  let makeLink kind =
    let make path manifest = Link { path; manifest; kind } in
    pathLike make

  let dist = Dist.parser >>| fun dist -> Dist dist
  let distRelaxed = Dist.parserRelaxed >>| fun dist -> Dist dist

  let link =
    withPrefix "link:" (makeLink LinkRegular ~requirePathSep:false)
    <|> withPrefix "link-dev:" (makeLink LinkDev ~requirePathSep:false)

  let parser = link <|> dist
  let parserRelaxed = link <|> distRelaxed
end

let parser = Parse.parser
let parserRelaxed = Parse.parserRelaxed

let parse =
  let open Parse in
  parse parser

let parseRelaxed =
  let open Parse in
  parse parserRelaxed

let to_yojson v = `String (show v)

let of_yojson json =
  match json with
  | `String string -> parse string
  | _ -> Error "expected string"

let relaxed_of_yojson json =
  match json with
  | `String string ->
      let parse =
        let open Parse in
        parse parserRelaxed
      in
      parse string
  | _ -> Error "expected string"

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
