type origin_info = Origin_info.t = InCST | Extra

val parse_ocaml: 
  Fpath.t -> (AST_ocaml.program, Parser_ml.token) Parsing_result.t

val parse_rust:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_jsonnet:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_yaml:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_bash:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_dockerfile:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

val parse_lisp:
  Fpath.t -> AST_generic.program * (Tok.t * origin_info) list

