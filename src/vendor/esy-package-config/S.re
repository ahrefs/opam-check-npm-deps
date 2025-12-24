module type PRINTABLE = {
  type t;

  let pp: Fmt.t(t);
  let show: t => string;
};

module type COMPARABLE = {
  type t;

  let compare: (t, t) => int;
};

module type COMMON = {
  type t;

  include COMPARABLE with type t := t;
  include PRINTABLE with type t := t;
};
