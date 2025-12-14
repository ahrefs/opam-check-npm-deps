type t =
  | Archive of { url : string; checksum : Checksum.t option }
  | Git of {
      remote : string;
      ref : string option;
      manifest : ManifestSpec.t option;
    }
  | Github of {
      user : string;
      repo : string;
      ref : string option;
      manifest : ManifestSpec.t option;
    }
  | LocalPath of Dist.local
  | NoSource
[@@deriving ord]

let show = function
  | Github { user; repo; ref = None; manifest = None } ->
      Printf.sprintf "github:%s/%s" user repo
  | Github { user; repo; ref = None; manifest = Some manifest } ->
      Printf.sprintf "github:%s/%s:%s" user repo (ManifestSpec.show manifest)
  | Github { user; repo; ref = Some ref; manifest = None } ->
      Printf.sprintf "github:%s/%s#%s" user repo ref
  | Github { user; repo; ref = Some ref; manifest = Some manifest } ->
      Printf.sprintf "github:%s/%s:%s#%s" user repo
        (ManifestSpec.show manifest)
        ref
  | Git { remote; ref = None; manifest = None } ->
      Printf.sprintf "git:%s" remote
  | Git { remote; ref = None; manifest = Some manifest } ->
      Printf.sprintf "git:%s:%s" remote (ManifestSpec.show manifest)
  | Git { remote; ref = Some ref; manifest = None } ->
      Printf.sprintf "git:%s#%s" remote ref
  | Git { remote; ref = Some ref; manifest = Some manifest } ->
      Printf.sprintf "git:%s:%s#%s" remote (ManifestSpec.show manifest) ref
  | Archive { url; checksum = Some checksum } ->
      Printf.sprintf "archive:%s#%s" url (Checksum.show checksum)
  | Archive { url; checksum = None } -> Printf.sprintf "archive:%s" url
  | LocalPath { path; manifest = None } ->
      Printf.sprintf "path:%s" (DistPath.show path)
  | LocalPath { path; manifest = Some manifest } ->
      Printf.sprintf "path:%s/%s" (DistPath.show path)
        (ManifestSpec.show manifest)
  | NoSource -> "no-source:"

let to_yojson src = `String (show src)
let pp fmt spec = Fmt.pf fmt "%s" (show spec)

let ofSource (src : Source.t) =
  match src with
  | Dist (Archive { url; checksum }) ->
      Archive { url; checksum = Some checksum }
  | Dist (Git { remote; commit; manifest }) ->
      Git { remote; ref = Some commit; manifest }
  | Dist (Github { user; repo; commit; manifest }) ->
      Github { user; repo; ref = Some commit; manifest }
  | Dist (LocalPath { path; manifest }) -> LocalPath { path; manifest }
  | Dist NoSource -> NoSource
  | Link { path; manifest; kind = _ } -> LocalPath { path; manifest }

module Parse = struct
  include Parse

  let manifestFilenameBeforeSharp = till (fun c -> c <> '#') ManifestSpec.parser

  let collectString xs =
    let l = List.length xs in
    let s = Bytes.create l in
    List.iteri ~f:(fun i c -> Bytes.set s i c) xs;
    Bytes.unsafe_to_string s

  let githubWithoutProto =
    let user = take_while1 (fun c -> c <> '/') in
    let repo =
      many_till any_char (string ".git")
      >>| collectString
      <|> take_while1 (fun c -> c <> '#' && c <> ':' && c <> '/')
    in
    let manifest = maybe (char ':' *> manifestFilenameBeforeSharp) in
    let ref = maybe (char '#' *> take_while1 (fun _ -> true)) in
    let make user repo manifest ref = Github { user; repo; ref; manifest } in
    make <$> (user <* char '/') <*> repo <*> manifest <*> ref

  let github =
    let prefix = string "github:" <|> string "gh:" in
    prefix *> githubWithoutProto

  let git =
    let prefix = string "git+" in
    let proto =
      let gitWithProto =
        prefix
        *> (string "https:" <|> string "http:" <|> string "ssh:"
          <|> string "ftp:" <|> string "rsync:")
      in
      gitWithProto <|> string "git:"
    in
    let remote = take_while1 (fun c -> c <> '#' && c <> ':') in
    let manifest = maybe (char ':' *> manifestFilenameBeforeSharp) in
    let ref = maybe (char '#' *> take_while1 (fun _ -> true)) in
    let make proto remote manifest ref =
      Git { remote = proto ^ remote; ref; manifest }
    in
    make <$> proto <*> remote <*> manifest <*> ref

  let archive =
    let proto = string "https:" <|> string "http:" in
    let url = take_while1 (fun c -> c <> '#') in
    let checksum = maybe (char '#' *> Checksum.parser) in
    let make proto url checksum = Archive { url = proto ^ url; checksum } in
    make <$> proto <*> url <*> checksum

  let pathWithoutProto make =
    let path = take_while1 (fun _ -> true) in
    let make path =
      let path =
        let open Path in
        normalizeAndRemoveEmptySeg (v path)
      in
      let path, manifest =
        match ManifestSpec.ofString (Path.basename path) with
        | Ok manifest ->
            let path =
              let open Path in
              remEmptySeg (parent path)
            in
            (path, Some manifest)
        | Error _ -> (path, None)
      in
      make (DistPath.ofPath path) manifest
    in
    make <$> path

  let pathLike proto make = string proto *> pathWithoutProto make

  let file =
    let make path manifest = LocalPath { path; manifest } in
    pathLike "file:" make

  let path =
    let make path manifest = LocalPath { path; manifest } in
    pathLike "path:" make

  let source =
    let source =
      let open Parse in
      github <|> git <|> archive <|> file <|> path <|> githubWithoutProto
    in
    let makePath path manifest = LocalPath { path; manifest } in
    peek_char_fail >>= function
    | '.' | '/' -> pathWithoutProto makePath
    | _ -> source
end

let parser = Parse.source

let parse =
  let open Parse in
  parse source

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
