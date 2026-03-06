(* stdlib.js *)
val path_stdlib : Common2.filename

(* generate stdlib.js *)
val extract_from_sources : Common2.filename list -> Common2.filename (* a dir *) -> unit
