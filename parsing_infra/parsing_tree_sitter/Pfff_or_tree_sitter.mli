(* Run pfff-based or tree-sitter-based parsers, or both, with
 * error recovery between the two to get the best results.
 *
 * This module does NOT depend on AST_generic or Parsing_result2,
 * so it can be used by language-specific graph builders (e.g., graph_code_c)
 * without pulling in all language parsers.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'ast parser =
  | Pfff of (Fpath.t -> 'ast * Parsing_stat.t)
  | TreeSitter of (Fpath.t -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

type 'ast internal_result =
  | ResOk of
      ('ast * Parsing_stat.t * Tree_sitter_run.Tree_sitter_error.t list)
  | ResPartial of
      ('ast * Parsing_stat.t * Tree_sitter_run.Tree_sitter_error.t list)
  | ResError of Exception.t

(* TODO: factorize with previous type *)
type 'ast pattern_parser =
  | PfffPat of (string -> 'ast)
  | TreeSitterPat of (string -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

val tree_sitter_only : bool ref
val pfff_only : bool ref
val debug_exn : bool ref

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* usage:
    run_raw file [
        TreeSitter Parse_c_tree_sitter.parse;
        Pfff (throw_tokens Parse_c.parse);
     ]
*)
val run_raw :
  Fpath.t ->
  'ast parser list ->
  'ast * Parsing_stat.t

(* Like run_raw but returns the full internal_result for callers
 * that need error details (e.g., to build Parsing_result2.t). *)
val run_either_filtered :
  Fpath.t ->
  'ast parser list ->
  'ast internal_result

val run_pattern : 'ast pattern_parser list -> string -> 'ast

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val exn_of_loc : Tok.location -> Exception.t

val loc_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Tok.location

val error_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Exception.t

val throw_tokens :
  (Fpath.t -> ('ast, 'toks) Parsing_result.t) ->
  Fpath.t ->
  'ast * Parsing_stat.t

val extract_pattern_from_tree_sitter_result :
  ('any, unit) Tree_sitter_run.Parsing_result.t -> 'any

val dump_and_print_errors :
  ('a -> unit) -> ('a, 'extra) Tree_sitter_run.Parsing_result.t -> unit
