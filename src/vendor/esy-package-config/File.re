type t = {
  root: Path.t,
  name: Path.t,
};

let pp = (fmt, file) =>
  Fmt.pf(fmt, "%a/%a", Path.pp, file.root, Path.pp, file.name);

let digest = file => {
  let path = Path.(file.root /\/ file.name);
  Digestv.ofFile(path);
};

let ofDir = base => {
  let rec loop = sub => {
    open RunAsync.Syntax;
    let root = Path.(base /\/ sub);
    let* exists = Fs.exists(root);
    if (exists) {
      let* files = Fs.listDir(root);
      let f = name => {
        let* isDir = Fs.isDir(Path.(root / name));
        if (isDir) {
          loop(Path.(sub / name));
        } else {
          return([
            {
              name: Path.(sub / name),
              root: base,
            },
          ]);
        };
      };
      let* lists = RunAsync.List.mapAndJoin(~concurrency=20, ~f, files);
      return(List.concat(lists));
    } else {
      return([]);
    };
  };

  loop(Path.v("."));
};

let placeAt = (path, file) => {
  open RunAsync.Syntax;
  let src = Path.(file.root /\/ file.name);
  let dst = Path.(path /\/ file.name);
  let () =
    Esy_logs.debug(m =>
      m(
        "Copying file from %s to %s",
        Path.showPretty(src),
        Path.showPretty(dst),
      )
    );
  let* () = Fs.createDir(Path.parent(dst));
  Fs.copyFile(~src, ~dst);
};
