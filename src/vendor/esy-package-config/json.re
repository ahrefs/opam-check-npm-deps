type t = Yojson.Safe.t;

type decoder('a) = t => result('a, string);
let parseJsonWith = (parser, json) => Run.ofStringError(parser(json));

