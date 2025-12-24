[@deriving ord]
type t = (kind, string)
and kind =
  | Md5
  | Sha1
  | Sha256
  | Sha512;

let _name = ((kind, _)) =>
  switch (kind) {
  | Md5 => "md5"
  | Sha1 => "sha1"
  | Sha256 => "sha256"
  | Sha512 => "sha512"
  };

let pp = (fmt, v) =>
  switch (v) {
  | (Md5, v) => Fmt.pf(fmt, "md5:%s", v)
  | (Sha1, v) => Fmt.pf(fmt, "sha1:%s", v)
  | (Sha256, v) => Fmt.pf(fmt, "sha256:%s", v)
  | (Sha512, v) => Fmt.pf(fmt, "sha512:%s", v)
  };

let show = v =>
  switch (v) {
  | (Md5, v) => "md5:" ++ v
  | (Sha1, v) => "sha1:" ++ v
  | (Sha256, v) => "sha256:" ++ v
  | (Sha512, v) => "sha512:" ++ v
  };

let parser = {
  open Parse;
  let md5 = ignore(string("md5")) <* char(':') >>| (() => Md5);
  let sha1 = ignore(string("sha1") <* char(':')) >>| (() => Sha1);
  let sha256 = ignore(string("sha256") <* char(':')) >>| (() => Sha256);
  let sha512 = ignore(string("sha512") <* char(':')) >>| (() => Sha512);
  let kind = md5 <|> sha1 <|> sha256 <|> sha512 <?> "kind";
  pair(option(Sha1, kind), hex) <?> "checksum";
};

let parse = Parse.parse(parser);

let to_yojson = v => `String(show(v));
let of_yojson = json =>
  switch (json) {
  | `String(v) => parse(v)
  | _ => Error("expected string")
  };

