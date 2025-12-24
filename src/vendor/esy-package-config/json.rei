type t = Yojson.Safe.t;

type decoder('a) = t => result('a, string);

let parseJsonWith: (decoder('a), t) => Run.t('a);
