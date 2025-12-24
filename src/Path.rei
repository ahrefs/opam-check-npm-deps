/**

  File system path.

 */

type t = Fpath.t;

let (/): (t, string) => t;

let currentPath: unit => t;

let showNormalized: t => string;

let show: t => string;
