type t('a) = Lwt.t(Result.t('a, string));

let return = v => Lwt.return(Ok(v));

let error = msg => Lwt.return(Result.error(msg));

let errorf = fmt => {
  let kerr = _ => Lwt.return(Result.error(Format.flush_str_formatter()));
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let bind = (~f, v) => {
  let waitForPromise =
    fun
    | Ok(v) => f(v)
    | Error(err) => Lwt.return(Error(err));

  Lwt.bind(v, waitForPromise);
};

module Syntax = {
  let return = return;
  let error = error;
  let errorf = errorf;
  let ( let* ) = (v, f) => bind(~f, v);
};

let runExn = (~err=?, v) => {
  switch (Lwt_main.run(v)) {
  | Ok(v) => v
  | Error(msg) =>
    let msg =
      switch (err) {
      | Some(err) => err ++ ": " ++ msg
      | None => msg
      };

    failwith(msg);
  };
};
