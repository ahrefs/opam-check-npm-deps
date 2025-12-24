type t = Fpath.t;

// in Fpath / is an alias to add_seg which fails if !is_seg
let (/) = (path, string) =>
  Fpath.(/\/)(path, Stdlib.Result.get_ok(string |> Fpath.of_string));

let currentPath = () =>
  switch (Bos.OS.Dir.current()) {
  | Ok(path) => path
  | Error(`Msg(msg)) =>
    failwith("Unable to determine current working dir: " ++ msg)
  };

let backSlashRegex = Str.regexp("\\\\");

let normalizePathSepOfFilename = p =>
  Str.global_replace(backSlashRegex, "/", p);

/* PRINTABLE */

let show = Fpath.to_string;
let pp = Fpath.pp;

let showNormalized = p => {
  let p = show(p);
  normalizePathSepOfFilename(p);
};
