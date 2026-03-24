
(* Uses both pfff (menhir) and tree-sitter parsers with fallback.
 * Adapted from Parse_target2.ml in semgrep.
 *)

(* Returns the full parsing result including errors and stats. *)
val parse_program_and_result: Lang.t -> Fpath.t -> Parsing_result2.t

(* Returns just the AST. *)
val parse_program: Lang.t -> Fpath.t -> AST_generic.program

(* This calls Naming_AST.ml to annotate the AST with naming information.
 * It essentially resolves local names.
 * To fully resolve, see Graph_code_generic.ml
 *)
val parse_and_resolve_name: Lang.t -> Fpath.t -> AST_generic.program
