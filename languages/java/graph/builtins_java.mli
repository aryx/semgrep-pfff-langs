(* will generate some java.lang.xxx.java files in dst by just extracting
 * the class definitions from src. Useful to build pfff/data/java_stdlib
 * Both src and dst must be directories.
 *)
val extract_from_sources :
  src:Fpath.t (* root *) -> dst:Common2.filename -> Common2.filename list -> unit
