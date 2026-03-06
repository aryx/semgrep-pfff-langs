(* Claude Code
 *
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
open AST_markdown
open Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighter for Markdown.
 *
 * Uses the AST to tag tokens with Highlight_code categories,
 * which are then rendered by codemap or efuns.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let heading_categ level =
  match level with
  | 1 -> CommentSection0
  | 2 -> CommentSection1
  | 3 -> CommentSection2
  | 4 -> CommentSection3
  | _ -> CommentSection4

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

let rec visit_inlines ~tag inlines =
  inlines |> List.iter (fun inline ->
    match inline with
    | Text _ -> ()
    | InlineCode (open_tok, (_s, ii), close_tok) ->
      tag open_tok Punctuation;
      tag ii EmbededCode;
      tag close_tok Punctuation
    | Bold (open_tok, inner, close_tok) ->
      tag open_tok Punctuation;
      tag close_tok Punctuation;
      visit_inlines ~tag inner
    | Italic (open_tok, inner, close_tok) ->
      tag open_tok Punctuation;
      tag close_tok Punctuation;
      visit_inlines ~tag inner
    | Link ((open_bracket, text_inner, close_bracket),
            (open_paren, (_url_s, url_tok), close_paren)) ->
      tag open_bracket Punctuation;
      visit_inlines ~tag text_inner;
      tag close_bracket Punctuation;
      tag open_paren Punctuation;
      tag url_tok EmbededUrl;
      tag close_paren Punctuation
    | Image (bang,
             (open_bracket, (_alt_s, alt_tok), close_bracket),
             (open_paren, (_url_s, url_tok), close_paren)) ->
      tag bang Punctuation;
      tag open_bracket Punctuation;
      tag alt_tok String;
      tag close_bracket Punctuation;
      tag open_paren Punctuation;
      tag url_tok EmbededUrl;
      tag close_paren Punctuation
    | HardLineBreak _ | SoftLineBreak _ -> ()
  )

let rec visit_block ~tag block =
  match block with
  | Heading (level, ii, inlines) ->
    let categ = heading_categ level in
    tag ii categ;
    inlines |> List.iter (fun inline ->
      match inline with
      | Text (_, tok) -> tag tok categ
      | _ -> ()
    );
    visit_inlines ~tag inlines
  | Paragraph inlines ->
    visit_inlines ~tag inlines
  | FencedCode ((_lang, lang_tok), (open_tok, (_code, code_tok), close_tok)) ->
    tag open_tok Keyword;
    tag lang_tok IncludeFilePath;
    tag code_tok EmbededCode;
    tag close_tok Keyword
  | IndentedCode (_s, ii) ->
    tag ii EmbededCode
  | Blockquote (ii, blocks) ->
    tag ii CommentImportance1;
    List.iter (visit_block ~tag) blocks
  | UnorderedList items | OrderedList items ->
    items |> List.iter (fun (ListItem (ii, blocks)) ->
      tag ii Punctuation;
      List.iter (visit_block ~tag) blocks
    )
  | ThematicBreak ii ->
    tag ii CommentEstet
  | BlankLine _ -> ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs _file (ast, _toks) =
  let tag ii categ = tag_hook ii categ in
  ast |> List.iter (visit_block ~tag)
