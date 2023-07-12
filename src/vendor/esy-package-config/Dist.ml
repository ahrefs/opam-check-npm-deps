open Sexplib0.Sexp_conv

type local = { path : DistPath.t; manifest : ManifestSpec.t option }
[@@deriving ord, sexp_of]

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
[@@deriving ord, sexp_of]

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
    [%bind
      match peek_char with
      | Some ':' | Some '%' ->
          let* () = ignore (string "%3A") <|> advance 1 in
          [%map
            let manifest =
              manifestFilenameBeforeSharp
              <|> fail "missing or incorrect <manifest>"
            in
            Some manifest]
      | _ -> return None]

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
    [%map
      let local = local ~requirePathSep in
      LocalPath local]

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

  [%%test_module
  let "Parse tests" =
    (module struct
      let test = Test.parse ~sexp_of:sexp_of_t (parse parser)
      let testRelaxed = Test.parse ~sexp_of:sexp_of_t (parse parserRelaxed)

      [%%expect_test
      let "no-source:" =
        test "no-source:";
        [%expect {|
           NoSource
         |}]]

      [%%expect_test
      let "path:/some/path" =
        test "path:/some/path";
        [%expect
          {|
           (LocalPath ((path /some/path) (manifest ())))
         |}]]

      [%%expect_test
      let "path:./some/path" =
        test "path:./some/path";
        [%expect
          {|
           (LocalPath ((path some/path) (manifest ())))
         |}]]

      [%%expect_test
      let "path:some" =
        test "path:some";
        [%expect
          {|
           (LocalPath ((path some) (manifest ())))
         |}]]

      [%%expect_test
      let "archive:http://example.com/pkg.tgz#abcdef" =
        test "archive:http://example.com/pkg.tgz#abdcdef";
        [%expect
          {|
           (Archive (url http://example.com/pkg.tgz) (checksum (Sha1 abdcdef)))
         |}]]

      [%%expect_test
      let "archive:https://example.com/pkg.tgz#abcdef" =
        test "archive:https://example.com/pkg.tgz#abdcdef";
        [%expect
          {|
           (Archive (url https://example.com/pkg.tgz) (checksum (Sha1 abdcdef)))
         |}]]

      [%%expect_test
      let "github:user/repo#abcdef" =
        test "github:user/repo#abcdef";
        [%expect
          {|
           (Github (user user) (repo repo) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "github:user/repo:manifest.opam#abcdef" =
        test "github:user/repo:manifest.opam#abcdef";
        [%expect
          {|
           (Github (user user) (repo repo) (commit abcdef)
            (manifest ((Opam manifest.opam))))
         |}]]

      [%%expect_test
      let "github:bryphe/lru:lru.opam#2708c70" =
        test "github:bryphe/lru:lru.opam#2708c70";
        [%expect
          {|
         (Github (user bryphe) (repo lru) (commit 2708c70)
          (manifest ((Opam lru.opam))))
       |}]]

      [%%expect_test
      let "github:bryphe/lru%3Alru.opam#2708c70" =
        test "github:bryphe/lru%3Alru.opam#2708c70";
        [%expect
          {|
          (Github (user bryphe) (repo lru) (commit 2708c70)
           (manifest ((Opam lru.opam))))
        |}]]

      [%%expect_test
      let "git:https://github.com/bryphe/lru.git%3Alru.opam#2708c70" =
        test "git:https://github.com/bryphe/lru.git%3Alru.opam#2708c70";
        [%expect
          {|
          (Git (remote https://github.com/bryphe/lru.git) (commit 2708c70)
           (manifest ((Opam lru.opam))))
        |}]]

      [%%expect_test
      let "git:https://github.com/esy/esy.git#abcdef" =
        test "git:https://github.com/esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git:https://github.com/esy/esy.git:esy.opam#abcdef" =
        test "git:https://github.com/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git+https://github.com/esy/esy.git#abcdef" =
        test "git:git+https://github.com/esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git:git+https://github.com/esy/esy.git:esy.opam#abcdef" =
        test "git:git+https://github.com/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:esy/esy.git#abcdef" =
        test "git:git+ssh://git@github.com:esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:esy/esy.git:esy.opam#abcdef" =
        test "git:git+ssh://git@github.com:esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git@github.com:esy/esy.git#abcdef" =
        test "git:git@github.com:esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git:git@github.com:esy/esy.git:esy.opam#abcdef" =
        test "git:git@github.com:esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git@github.com:esy/esy.git#abcdef" =
        test "git:git@github.com:esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git:git@github.com:esy/esy.git:esy.opam#abcdef" =
        test "git:git@github.com:esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git+https://github.com:8080/esy/esy.git#abcdef" =
        test "git:git+https://github.com:8080/esy/esy.git#abcdef";
        [%expect
          {|
        (Git (remote https://github.com:8080/esy/esy.git) (commit abcdef)
         (manifest ()))
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git#abcdef" =
        test "git:git+ssh://git@github.com:22/esy/esy.git#abcdef";
        [%expect
          {|
       (Git (remote git@github.com:22/esy/esy.git) (commit abcdef) (manifest ()))
     |}]]

      [%%expect_test
      let "git:git+https://github.com:8080/esy/esy.git:esy.opam#abcdef" =
        test "git:git+https://github.com:8080/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
       (Git (remote https://github.com:8080/esy/esy.git) (commit abcdef)
        (manifest ((Opam esy.opam))))
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git:esy.opam#abcdef" =
        test "git:git+ssh://git@github.com:22/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
       (Git (remote git@github.com:22/esy/esy.git) (commit abcdef)
        (manifest ((Opam esy.opam))))
       |}]]

      [%%expect_test
      let "http://example.com/pkg.tgz#abcdef" =
        testRelaxed "http://example.com/pkg.tgz#abdcdef";
        [%expect
          {|
           (Archive (url http://example.com/pkg.tgz) (checksum (Sha1 abdcdef)))
         |}]]

      [%%expect_test
      let "https://example.com/pkg.tgz#abcdef" =
        testRelaxed "https://example.com/pkg.tgz#abdcdef";
        [%expect
          {|
           (Archive (url https://example.com/pkg.tgz) (checksum (Sha1 abdcdef)))
         |}]]

      [%%expect_test
      let "user/repo#abcdef" =
        testRelaxed "user/repo#abcdef";
        [%expect
          {|
           (Github (user user) (repo repo) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "user/repo:manifest.opam#abcdef" =
        testRelaxed "user/repo:manifest.opam#abcdef";
        [%expect
          {|
           (Github (user user) (repo repo) (commit abcdef)
            (manifest ((Opam manifest.opam))))
         |}]]

      [%%expect_test
      let "git:https://github.com/bryphe/lru.git%3Alru.opam#2708c70" =
        testRelaxed "git:https://github.com/bryphe/lru.git%3Alru.opam#2708c70";
        [%expect
          {|
         (Git (remote https://github.com/bryphe/lru.git) (commit 2708c70)
          (manifest ((Opam lru.opam))))
       |}]]

      [%%expect_test
      let "git+https://github.com/esy/esy.git#abcdef" =
        testRelaxed "git+https://github.com/esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git+https://github.com/esy/esy.git:esy.opam#abcdef" =
        testRelaxed "git+https://github.com/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote https://github.com/esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git+http://github.com/esy/esy.git#abcdef" =
        testRelaxed "git+http://github.com/esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote http://github.com/esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git+http://github.com/esy/esy.git:esy.opam#abcdef" =
        testRelaxed "git+http://github.com/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote http://github.com/esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git://github.com/esy/esy.git#abcdef" =
        testRelaxed "git://github.com/esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote git://github.com/esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git://github.com/esy/esy.git:esy.opam#abcdef" =
        testRelaxed "git://github.com/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote git://github.com/esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git+ssh://git@github.com:esy/esy.git#abcdef" =
        testRelaxed "git+ssh://git@github.com:esy/esy.git#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef) (manifest ()))
         |}]]

      [%%expect_test
      let "git+ssh://git@github.com:esy/esy.git:esy.opam#abcdef" =
        testRelaxed "git+ssh://git@github.com:esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
           (Git (remote git@github.com:esy/esy.git) (commit abcdef)
            (manifest ((Opam esy.opam))))
         |}]]

      [%%expect_test
      let "git:git+https://github.com:8080/esy/esy.git#abcdef" =
        testRelaxed "git:git+https://github.com:8080/esy/esy.git#abcdef";
        [%expect
          {|
        (Git (remote https://github.com:8080/esy/esy.git) (commit abcdef)
         (manifest ()))
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git#abcdef" =
        testRelaxed "git:git+ssh://git@github.com:22/esy/esy.git#abcdef";
        [%expect
          {|
       (Git (remote git@github.com:22/esy/esy.git) (commit abcdef) (manifest ()))
     |}]]

      [%%expect_test
      let "git:git+https://github.com:8080/esy/esy.git:esy.opam#abcdef" =
        testRelaxed
          "git:git+https://github.com:8080/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
       (Git (remote https://github.com:8080/esy/esy.git) (commit abcdef)
        (manifest ((Opam esy.opam))))
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git:esy.opam#abcdef" =
        testRelaxed
          "git:git+ssh://git@github.com:22/esy/esy.git:esy.opam#abcdef";
        [%expect
          {|
       (Git (remote git@github.com:22/esy/esy.git) (commit abcdef)
        (manifest ((Opam esy.opam))))
       |}]]

      [%%expect_test
      let "github:user/repo#ref" =
        test "github:user/repo#ref";
        [%expect
          {|
      ERROR: parsing "github:user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user/repo#" =
        test "github:user/repo#";
        [%expect
          {|
      ERROR: parsing "github:user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user/repo:#abc123" =
        test "github:user/repo:#abc123";
        [%expect
          {|
      ERROR: parsing "github:user/repo:#abc123": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <manifest>
      |}]]

      [%%expect_test
      let "github:user/repo" =
        test "github:user/repo";
        [%expect
          {|
      ERROR: parsing "github:user/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user" =
        test "github:user";
        [%expect
          {|
      ERROR: parsing "github:user": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:/repo" =
        test "github:/repo";
        [%expect
          {|
      ERROR: parsing "github:/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:user/" =
        test "github:user/";
        [%expect
          {|
      ERROR: parsing "github:user/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:/" =
        test "github:/";
        [%expect
          {|
      ERROR: parsing "github:/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:" =
        test "github:";
        [%expect
          {|
      ERROR: parsing "github:": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "git:https://example.com#ref" =
        test "git:https://example.com#ref";
        [%expect
          {|
      ERROR: parsing "git:https://example.com#ref": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:https://example.com#" =
        test "git:https://example.com#";
        [%expect
          {|
      ERROR: parsing "git:https://example.com#": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:https://example.com" =
        test "git:https://example.com";
        [%expect
          {|
      ERROR: parsing "git:https://example.com": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:" =
        test "git:";
        [%expect
          {|
      ERROR: parsing "git:": <remote>(:<manifest>)?#<commit>: missing on incorrect <remote>
      |}]]

      [%%expect_test
      let "archive:https://example.com#gibberish" =
        test "archive:https://example.com#gibberish";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com#md5:gibberish" =
        test "archive:https://example.com#md5:gibberish";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com#" =
        test "archive:https://example.com#";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com" =
        test "archive:https://example.com";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:ftp://example.com" =
        test "archive:ftp://example.com";
        [%expect
          {|
      ERROR: parsing "archive:ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git#abcdef" =
        test "git:git+ssh://git@github.com:22:esy/esy.git#abcdef";
        [%expect
          {|
          ERROR: parsing "git:git+ssh://git@github.com:22:esy/esy.git#abcdef": <remote>(:<manifest>)?#<commit>: missing or incorrect <manifest>
     |}]]

      [%%expect_test
      let "github:user/repo#ref" =
        testRelaxed "github:user/repo#ref";
        [%expect
          {|
      ERROR: parsing "github:user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user/repo#" =
        testRelaxed "github:user/repo#";
        [%expect
          {|
      ERROR: parsing "github:user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user/repo:#abc123" =
        testRelaxed "github:user/repo:#abc123";
        [%expect
          {|
      ERROR: parsing "github:user/repo:#abc123": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <manifest>
      |}]]

      [%%expect_test
      let "github:user/repo" =
        testRelaxed "github:user/repo";
        [%expect
          {|
      ERROR: parsing "github:user/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "github:user" =
        testRelaxed "github:user";
        [%expect
          {|
      ERROR: parsing "github:user": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:/repo" =
        testRelaxed "github:/repo";
        [%expect
          {|
      ERROR: parsing "github:/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:user/" =
        testRelaxed "github:user/";
        [%expect
          {|
      ERROR: parsing "github:user/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:/" =
        testRelaxed "github:/";
        [%expect
          {|
      ERROR: parsing "github:/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "github:" =
        testRelaxed "github:";
        [%expect
          {|
      ERROR: parsing "github:": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |}]]

      [%%expect_test
      let "git:https://example.com#ref" =
        testRelaxed "git:https://example.com#ref";
        [%expect
          {|
      ERROR: parsing "git:https://example.com#ref": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:https://example.com#" =
        testRelaxed "git:https://example.com#";
        [%expect
          {|
      ERROR: parsing "git:https://example.com#": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:https://example.com" =
        testRelaxed "git:https://example.com";
        [%expect
          {|
      ERROR: parsing "git:https://example.com": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:" =
        testRelaxed "git:";
        [%expect
          {|
      ERROR: parsing "git:": <remote>(:<manifest>)?#<commit>: missing on incorrect <remote>
      |}]]

      [%%expect_test
      let "archive:https://example.com#gibberish" =
        testRelaxed "archive:https://example.com#gibberish";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com#md5:gibberish" =
        testRelaxed "archive:https://example.com#md5:gibberish";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com#" =
        testRelaxed "archive:https://example.com#";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:https://example.com" =
        testRelaxed "archive:https://example.com";
        [%expect
          {|
      ERROR: parsing "archive:https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "archive:ftp://example.com" =
        testRelaxed "archive:ftp://example.com";
        [%expect
          {|
      ERROR: parsing "archive:ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |}]]

      [%%expect_test
      let "https://example.com#gibberish" =
        testRelaxed "https://example.com#gibberish";
        [%expect
          {|
      ERROR: parsing "https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "https://example.com#md5:gibberish" =
        testRelaxed "https://example.com#md5:gibberish";
        [%expect
          {|
      ERROR: parsing "https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "https://example.com#" =
        testRelaxed "https://example.com#";
        [%expect
          {|
      ERROR: parsing "https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "https://example.com" =
        testRelaxed "https://example.com";
        [%expect
          {|
      ERROR: parsing "https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |}]]

      [%%expect_test
      let "ftp://example.com" =
        testRelaxed "ftp://example.com";
        [%expect
          {|
      ERROR: parsing "ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |}]]

      [%%expect_test
      let "user/repo#ref" =
        testRelaxed "user/repo#ref";
        [%expect
          {|
      ERROR: parsing "user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "user/repo#" =
        testRelaxed "user/repo#";
        [%expect
          {|
      ERROR: parsing "user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |}]]

      [%%expect_test
      let "git:git+ssh://git@github.com:22/esy/esy.git#abcdef" =
        testRelaxed "git:git+ssh://git@github.com:22:esy/esy.git#abcdef";
        [%expect
          {|
          ERROR: parsing "git:git+ssh://git@github.com:22:esy/esy.git#abcdef": <remote>(:<manifest>)?#<commit>: missing or incorrect <manifest>
     |}]]
    end)]
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
