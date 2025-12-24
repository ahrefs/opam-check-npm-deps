/**

  File system path.

 */

type t = Fpath.t;

let (/): (t, string) => t;

let currentPath: unit => t;

let showNormalized: t => string;

include S.PRINTABLE with type t := t;
