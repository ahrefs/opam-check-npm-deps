/**
 * Computation with structured error reporting.
 */

type t('a) = result('a, error)
and error = (string, context)
and context = list(contextItem)
and contextItem =
  | Line(string)
  | LogOutput((string, string));

let ppContextItem = fmt =>
  fun
  | Line(line) => Fmt.pf(fmt, "@[<h>%s@]", line)
  | [@implicit_arity] LogOutput(filename, out) =>
    Fmt.pf(fmt, "@[<h 2>%s@\n%a@]", filename, Fmt.text, out);

let ppContext = (fmt, context) =>
  Fmt.(list(~sep=any("@\n"), ppContextItem))(fmt, List.rev(context));

let ppError = (fmt, (msg, context)) =>
  Fmt.pf(
    fmt,
    "@[<v 2>@[<h>error: %a@]@\n%a@]",
    Fmt.text,
    msg,
    ppContext,
    context,
  );

let return = v => Ok(v);

let error = msg => [@implicit_arity] Error(msg, []);

let errorf = fmt => {
  let kerr = _ => [@implicit_arity] Error(Format.flush_str_formatter(), []);
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let context = (line, v) =>
  switch (v) {
  | Ok(v) => Ok(v)
  | Error((msg, context)) => Error((msg, [Line(line), ...context]))
  };

let ofStringError = v =>
  switch (v) {
  | Ok(v) => Ok(v)
  | Error(line) => Error((line, []))
  };

let ofBosError = v =>
  switch (v) {
  | Ok(v) => Ok(v)
  | Error(`Msg(line)) => Error((line, []))
  | Error(`CommandError(cmd, status)) =>
    let line =
      Format.asprintf(
        "command %a exited with status %a",
        Bos.Cmd.pp,
        cmd,
        Bos.OS.Cmd.pp_status,
        status,
      );

    Error((line, []));
  };

let formatError = error => Format.asprintf("%a", ppError, error);
let runExn = (~err=?) =>
  fun
  | Ok(v) => v
  | [@implicit_arity] Error(msg, ctx) => {
      let msg =
        switch (err) {
        | Some(err) => err ++ ": " ++ msg
        | None => msg
        };

      failwith(formatError((msg, ctx)));
    };
