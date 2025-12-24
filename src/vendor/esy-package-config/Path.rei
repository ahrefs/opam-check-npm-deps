/**

  File system path.

 */

type t = Fpath.t;
type ext = Fpath.ext;

let v: string => t;
let (/): (t, string) => t;
let (/\/): (t, t) => t;

let addSeg: (t, string) => t;
let append: (t, t) => t;

let ofString: string => result(t, [> | `Msg(string)]);

let currentPath: unit => t;

let isPrefix: (t, t) => bool;

let isAbs: t => bool;
let basename: t => string;
let parent: t => t;
let relativize: (~root: t, t) => option(t);
let tryRelativize: (~root: t, t) => t;
let tryRelativizeToCurrent: t => t;

let addExt: (ext, t) => t;
let hasExt: (ext, t) => bool;
let remExt: (~multi: bool=?, t) => t;
let getExt: (~multi: bool=?, t) => ext;

let remExtOfFilename: (~multi: bool=?, string) => string;

let dirSep: string;

let showNormalized: t => string;

include S.PRINTABLE with type t := t;
include S.COMPARABLE with type t := t;
include S.JSONABLE with type t := t;

module Set: (module type of Fpath.Set);

let safeSeg: string => string;
let safePath: string => string;

let remEmptySeg: t => t;
let normalize: t => t;
let normalizePathSepOfFilename: string => string;
let normalizeAndRemoveEmptySeg: t => t;

let equal: (t, t) => bool;
