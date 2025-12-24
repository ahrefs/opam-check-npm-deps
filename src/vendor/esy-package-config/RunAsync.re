type t('a) = Lwt.t(Run.t('a));

let return = v => Lwt.return(Ok(v));

let error = msg => Lwt.return(Run.error(msg));

let errorf = fmt => {
  let kerr = _ => Lwt.return(Run.error(Format.flush_str_formatter()));
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let bind = (~f, v) => {
  let waitForPromise =
    fun
    | Ok(v) => f(v)
    | Error(err) => Lwt.return(Error(err));

  Lwt.bind(v, waitForPromise);
};

let ofRun = Lwt.return;

module Syntax = {
  let return = return;
  let error = error;
  let errorf = errorf;
  let ( let* ) = (v, f) => bind(~f, v);
};

let runExn = (~err=?, v) => {
  let v = Lwt_main.run(v);
  Run.runExn(~err?, v);
};
