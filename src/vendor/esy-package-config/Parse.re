include Angstrom;

module Let_syntax = {
  let map = (~f, p) => p >>| f;
  let bind = (~f, p) => p >>= f;
};

let (>>) = (a, b) => a >>= (() => b);

let ignore = p => p >>| (_ => ());

let const = (v, _) => return(v);

let maybe = p => option(None, p >>| (v => Some(v)));

let failIf = (msg, p) => {
  option(false, p >> return(true))
  >>= (
    fun
    | true => {
        fail(msg);
      }
    | false => {
        return();
      }
  );
};

let till = (c, p) => {
  let* input = take_while1(c);
  switch (parse_string(~consume=All, p, input)) {
  | Ok(fname) => return(fname)
  | Error(msg) => fail(msg)
  };
};

let pair = {
  let pair = (x, y) => (x, y);
  (a, b) => lift2(pair, a, b);
};

let hex =
  take_while1(
    fun
    | '0' .. '9' => true
    | 'a'
    | 'b'
    | 'c'
    | 'd'
    | 'e'
    | 'f' => true
    | _ => false,
  )
  <?> "hex";

let parse = (p, input) =>
  switch (parse_string(~consume=All, p <* end_of_input, input)) {
  | Ok(v) => Ok(v)
  | Error(msg) =>
    let msg = Printf.sprintf({|parsing "%s": %s|}, input, msg);
    Error(msg);
  };
