type local = { path : DistPath.t; manifest : ManifestSpec.t option }
[@@deriving ord]

type t =
  | Archive of { url : string; checksum : Checksum.t }
  | Git of {
      remote : string;
      commit : string;
      manifest : ManifestSpec.t option;
    }
  | Github of {
      user : string;
      repo : string;
      commit : string;
      manifest : ManifestSpec.t option;
    }
  | LocalPath of local
  | NoSource
[@@deriving ord]

let manifest (dist : t) =
  match dist with
  | Git { manifest = Some manifest; _ } -> Some manifest
  | Git _ -> None
  | Github { manifest = Some manifest; _ } -> Some manifest
  | Github _ -> None
  | LocalPath info -> info.manifest
  | Archive _ -> None
  | NoSource -> None

let show' ~showPath = function
  | Github { user; repo; commit; manifest = None } ->
      Printf.sprintf "github:%s/%s#%s" user repo commit
  | Github { user; repo; commit; manifest = Some manifest } ->
      Printf.sprintf "github:%s/%s:%s#%s" user repo
        (ManifestSpec.show manifest)
        commit
  | Git { remote; commit; manifest = None } ->
      Printf.sprintf "git:%s#%s" remote commit
  | Git { remote; commit; manifest = Some manifest } ->
      Printf.sprintf "git:%s:%s#%s" remote (ManifestSpec.show manifest) commit
  | Archive { url; checksum } ->
      Printf.sprintf "archive:%s#%s" url (Checksum.show checksum)
  | LocalPath { path; manifest = None } ->
      Printf.sprintf "path:%s" (showPath path)
  | LocalPath { path; manifest = Some manifest } ->
      Printf.sprintf "path:%s/%s" (showPath path) (ManifestSpec.show manifest)
  | NoSource -> "no-source:"

let show = show' ~showPath:DistPath.show
let showPretty = show' ~showPath:DistPath.showPretty
let pp fmt src = Fmt.pf fmt "%s" (show src)
let ppPretty fmt src = Fmt.pf fmt "%s" (showPretty src)

module Parse = struct
  include Parse

  let manifestFilenameBeforeSharp = till (fun c -> c <> '#') ManifestSpec.parser

  let urlWithPort =
    let* host = take_while1 (fun c -> c <> ':') <* char ':' in
    let* port =
      take_while1 (fun c -> c <> '/') >>= fun p ->
      match int_of_string_opt p with
      | Some port -> return (string_of_int port)
      | None -> fail "invalid port"
    in
    let* path = take_while1 (fun c -> c <> '#' && c <> ':' && c <> '%') in
    return (host ^ ":" ^ port ^ path)

  let commitSHA =
    let err = fail "missing or incorrect <commit>" in
    let* () = ignore (char '#') <|> err in
    let* () = commit in
    let* sha = hex <|> err in
    if String.length sha < 6 then err else return sha

  let gitOrGithubManifest =
    peek_char >>= function
    | Some ':' | Some '%' ->
        let* () = ignore (string "%3A") <|> advance 1 in
        manifestFilenameBeforeSharp <|> fail "missing or incorrect <manifest>"
        >>| fun manifest -> Some manifest
    | _ -> return None

  let github =
    (let* user =
       take_while1 (fun c -> c <> '/')
       <* char '/'
       <|> fail "missing or incorrect <author>/<repo>"
     in
     let* repo =
       take_while1 (fun c -> c <> '#' && c <> ':' && c <> '%')
       <|> fail "missing or incorrect <author>/<repo>"
     in
     let* manifest = gitOrGithubManifest in
     let* commit = commitSHA in
     return (Github { user; repo; commit; manifest }))
    <?> "<author>/<repo>(:<manifest>)?#<commit>"

  let gitViaSSH =
    let* parts =
      sep_by1 (char ':') (take_while1 (fun c -> c <> ':' && c <> '#'))
    in
    match parts with
    | [] -> fail "missing or incorrect <remote>"
    | remote :: [] ->
        let* commit = commitSHA in
        return (Git { remote; commit; manifest = None })
    | [ remote; path ] ->
        let* commit = commitSHA in
        return (Git { remote = remote ^ ":" ^ path; commit; manifest = None })
    | [ remote; path; manifest ] ->
        let* commit = commitSHA in
        let* manifest =
          match ManifestSpec.ofString manifest with
          | Ok manifest -> return manifest
          | Error err -> fail ("invalid manifest: " ^ err)
        in
        return
          (Git
             { remote = remote ^ ":" ^ path; commit; manifest = Some manifest })
    | _ -> fail "invalid remote"

  let gitRelaxed =
    let viaURL =
      let* proto =
        let gitproto = string "git://" *> return "git://" in
        let httpproto = string "git+http://" *> return "http://" in
        let httpsproto = string "git+https://" *> return "https://" in
        gitproto <|> httpproto <|> httpsproto
      in
      let* () = commit in
      let* remote =
        choice ~failure_msg:"missing on incorrect <remote>"
          [
            urlWithPort; take_while1 (fun c -> c <> '#' && c <> ':' && c <> '%');
          ]
      in
      let* manifest = gitOrGithubManifest in
      let* commit = commitSHA in
      return (Git { remote = proto ^ remote; commit; manifest })
    in
    let viaSSH = string "git+ssh://" *> commit *> gitViaSSH in
    viaURL <|> viaSSH

  let git =
    (let* () =
       ignore (string "git+ssh://") <|> ignore (string "git+") <|> return ()
     in
     let* proto =
       take_while1 (fun c -> c <> ':')
       <* char ':'
       <|> fail "missing on incorrect <remote>"
     in
     let* remote =
       choice ~failure_msg:"missing on incorrect <remote>"
         [
           urlWithPort; take_while1 (fun c -> c <> '#' && c <> ':' && c <> '%');
         ]
     in
     let* manifest = gitOrGithubManifest in
     let* commit = commitSHA in
     return (Git { remote = proto ^ ":" ^ remote; commit; manifest }))
    <?> "<remote>(:<manifest>)?#<commit>"

  let archive =
    (let* proto =
       take_while1 (fun c -> c <> ':')
       <* string "://"
       <|> fail "missing on incorrect <remote>"
     in
     let* () = commit in
     let* proto =
       match proto with
       | "http" | "https" -> return proto
       | _ -> fail "incorrect protocol: expected http: or https:"
     in
     let* host = take_while1 (fun c -> c <> '#') in
     let* checksum =
       char '#' *> Checksum.parser <|> fail "missing or incorrect <checksum>"
     in
     return (Archive { url = proto ^ "://" ^ host; checksum }))
    <?> "https?://<host>/<path>#<checksum>"

  let local ~requirePathSep =
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
      { path = DistPath.ofPath path; manifest }
    in
    let path =
      scan false (fun seenPathSep c -> Some (seenPathSep || c = '/'))
    in
    let* path, seenPathSep = path in
    if (not requirePathSep) || seenPathSep then return (make path)
    else fail "not a path"

  let path ~requirePathSep =
    local ~requirePathSep >>| fun local -> LocalPath local

  let schema =
    let git =
      ignore (string "git:")
      *> failIf "invalid dependency schema" (ignore (string "//"))
      *> return `Git
    in
    git
    <|> (string "github:" >>= const `GitHub)
    <|> (string "gh:" >>= const `GitHub)
    <|> (string "archive:" >>= const `Archive)
    <|> (string "path:" >>= const `Path)
    <|> (string "no-source:" >>= const `NoSource)

  let parser =
    let* schema = schema in
    let* () = commit in
    match schema with
    | `Git -> git
    | `GitHub -> github
    | `Archive -> archive
    | `Path -> path ~requirePathSep:false
    | `NoSource -> return NoSource

  let parserRelaxed =
    parser <|> gitRelaxed <|> archive <|> github <|> path ~requirePathSep:true
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

let local_of_yojson json =
  match json with
  | `String string ->
      let parse =
        let open Parse in
        parse (local ~requirePathSep:false)
      in
      parse string
  | _ -> Error "expected string"

let local_to_yojson local =
  match local.manifest with
  | None -> `String (DistPath.show local.path)
  | Some m ->
      `String
        (let open DistPath in
         show (local.path / ManifestSpec.show m))

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)
