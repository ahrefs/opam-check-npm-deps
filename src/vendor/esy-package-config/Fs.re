let toRunAsync = (~desc="I/O failed", promise) => {
  open RunAsync.Syntax;
  try%lwt(
    {
      let%lwt v = promise();
      return(v);
    }
  ) {
  | Unix.Unix_error(err, _, _) =>
    let msg = Unix.error_message(err);
    error(Printf.sprintf("%s: %s", desc, msg));
  };
};

let readFile = (path: Path.t) => {
  let path = Path.show(path);
  let desc = Printf.sprintf("Unable to read file %s", path);
  toRunAsync(
    ~desc,
    () => {
      let f = ic => Lwt_io.read(ic);
      Lwt_io.with_file(~mode=Lwt_io.Input, path, f);
    },
  );
};


let readJsonFile = (path: Path.t) => {
  open RunAsync.Syntax;
  let* data = readFile(path);
  try(return(Yojson.Safe.from_string(data))) {
  | Yojson.Json_error(msg) =>
    errorf("error reading JSON file: %a@\n%s", Path.pp, path, msg)
  };
};

