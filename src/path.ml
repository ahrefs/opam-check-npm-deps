type t = Fpath.t

let ( / ) path =
 fun string ->
  Fpath.( // ) path (Stdlib.Result.get_ok (string |> Fpath.of_string))

let currentPath () =
  match Bos.OS.Dir.current () with
  | Ok path -> path
  | Error (`Msg msg) ->
      failwith ("Unable to determine current working dir: " ^ msg)

let show = Fpath.to_string

let showNormalized =
  let backSlashRegex = Str.regexp "\\\\" in
  let normalizePathSepOfFilename p = Str.global_replace backSlashRegex "/" p in
  fun p ->
    let p = show p in
    normalizePathSepOfFilename p
