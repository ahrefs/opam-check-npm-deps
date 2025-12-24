type t('a) = Lwt.t(Run.t('a));

let return = v => Lwt.return(Ok(v));

let error = msg => Lwt.return(Run.error(msg));

let errorf = fmt => {
  let kerr = _ => Lwt.return(Run.error(Format.flush_str_formatter()));
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let context = msg => {
  Lwt.map(Run.context(msg));
};

let contextf = (v, fmt) => {
  let kerr = _ => context(Format.flush_str_formatter(), v);
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let map = (~f, v) => {
  let waitForPromise =
    fun
    | Ok(v) => Lwt.return(Ok(f(v)))
    | Error(err) => Lwt.return(Error(err));

  Lwt.bind(v, waitForPromise);
};

let bind = (~f, v) => {
  let waitForPromise =
    fun
    | Ok(v) => f(v)
    | Error(err) => Lwt.return(Error(err));

  Lwt.bind(v, waitForPromise);
};

let both = (a, b) => {
  Lwt.bind(a, a =>
    Lwt.map(
      b =>
        switch (a, b) {
        | (Ok(a), Ok(b)) => [@implicit_arity] Ok(a, b)
        | (Ok(_), Error(err)) => Error(err)
        | (Error(err), Ok(_)) => Error(err)
        | (Error(err), Error(_)) => Error(err)
        },
      b,
    )
  );
};

let ofRun = Lwt.return;
let ofLwt = lwt => Lwt.bind(lwt, v => Lwt.return(Ok(v)));
let ofStringError = r => ofRun(Run.ofStringError(r));
let ofBosError = r => ofRun(Run.ofBosError(r));

let try_ = (~catch, computation) => {
  let open Lwt.Infix;
  computation >>= fun
  | Ok(value) => return(value)
  | Error(error) => catch(error);
};

module Syntax = {
  let return = return;
  let error = error;
  let errorf = errorf;
  let ( let* ) = (v, f) => bind(~f, v);

  module Let_syntax = {
    let map = map;
    let bind = bind;
    let both = both;
  };
};

let ofOption = (~err=?, v) =>
  switch (v) {
  | Some(v) => return(v)
  | None =>
    let err =
      switch (err) {
      | Some(err) => err
      | None => "not found"
      };
    error(err);
  };

let runExn = (~err=?, v) => {
  let v = Lwt_main.run(v);
  Run.runExn(~err?, v);
};
