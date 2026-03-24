(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open Pfff_or_tree_sitter

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Dispatcher to different semgrep-pfff-langs parsers.
 *
 * Adapted from Parse_target2.ml in semgrep to use both pfff (menhir) and
 * tree-sitter parsers with fallback between them.
 *
 * history:
 *  - this was in pfff/lang_GENERIC/parse_generic.ml
 *  - this was moved in semgrep and generalized to Parse_target.ml to also
 *    use tree-sitter parsers
 *  - this was move back to codegraph and restricted back to pfff parsers to be
 *    used in codegraph/codecheck without depending on semgrep-core
 *  - moved to semgrep-pfff-langs/parsing/
 *  - added tree-sitter support back, adapted from Parse_target2.ml in semgrep
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lang_to_python_parsing_mode = function
  | Lang.Python -> Parse_python.Python
  | Lang.Python2 -> Parse_python.Python2
  | Lang.Python3 -> Parse_python.Python3
  | s -> failwith (spf "not a python language:%s" (Lang.to_string s))

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* Adapted from Parse_target2.just_parse_with_lang in semgrep *)
let parse_program_and_result (lang : Lang.t) (file : Fpath.t)
    : Parsing_result2.t =
  match lang with
  (* Menhir and Tree-sitter *)
  | Lang.C ->
      run file
        [
          Pfff (throw_tokens Parse_c.parse);
          TreeSitter Parse_c_tree_sitter.parse;
        ]
        C_to_generic.program
  | Lang.Java ->
      run file
        [
          TreeSitter Parse_java_tree_sitter.parse;
          Pfff (throw_tokens Parse_java.parse);
        ]
        Java_to_generic.program
  | Lang.Js ->
      run file
        [
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
  | Lang.Ts ->
      run file
        [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None) ]
        Js_to_generic.program
  | Lang.Ocaml ->
      run file
        [
          TreeSitter Parse_ocaml_tree_sitter.parse;
          Pfff (throw_tokens Parse_ml.parse);
        ]
        Ocaml_to_generic.program
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      let parsing_mode = lang_to_python_parsing_mode lang in
      run file
        [
          Pfff (throw_tokens (Parse_python.parse ~parsing_mode));
          TreeSitter Parse_python_tree_sitter.parse;
        ]
        (Python_to_generic.program ~assign_to_vardef:true)
  | _ -> failwith (spf "lang %s not supported yet" (Lang.to_string lang))

let parse_program (lang : Lang.t) (file : Fpath.t) =
  let res = parse_program_and_result lang file in
  res.Parsing_result2.ast

(* coupling: mostly a copy paste of
 * Parse_target.parse_and_resolve_name_use_pfff_or_treesitter in semgrep-core *)
let parse_and_resolve_name (lang : Lang.t) (file : Fpath.t) =
  let ast = parse_program lang file in
  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  (* TODO? AST_generic.SId.unsafe_reset_counter(); *)
  Naming_AST.resolve lang ast;
  ast
