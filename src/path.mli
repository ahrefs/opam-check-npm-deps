type t = Fpath.t

val ( / ) : t -> string -> t
val currentPath : unit -> t
val showNormalized : t -> string
val show : t -> string
