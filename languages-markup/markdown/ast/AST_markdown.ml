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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for Markdown (CommonMark subset).
 *
 * Reference: https://spec.commonmark.org/
 *
 * Three distinctive Markdown features reflected in this AST:
 *
 * 1. ATX headings with levels: # through ###### prefix lines to
 *    create hierarchical section structure.
 *    AST: Heading with level and inline content.
 *
 * 2. Inline formatting with delimiter runs: **bold**, *italic*,
 *    `code`, [links](url), ![images](url) can nest.
 *    AST: inline type with Bold, Italic, Code, Link, Image
 *    using bracket for paired delimiters.
 *
 * 3. Fenced code blocks with language hints: ``` or ~~~ delimit
 *    verbatim code blocks, with an optional language tag.
 *    AST: FencedCode with bracket for the fences.
 *)

(*****************************************************************************)
(* Token info *)
(*****************************************************************************)

type tok = Tok.t [@@deriving show]
type 'a wrap = 'a * tok [@@deriving show]
(* opening delimiter, content, closing delimiter *)
type 'a bracket = tok * 'a * tok [@@deriving show]

(*****************************************************************************)
(* AST definition *)
(*****************************************************************************)

type document = block list [@@deriving show]

and block =
  (* ## Heading text; int is the level 1-6, tok is the '#' sequence *)
  | Heading of int * tok * inline list
  | Paragraph of inline list
  (* ```lang\ncode\n```; string option wrap is the language tag *)
  | FencedCode of string option wrap * string wrap bracket
  | IndentedCode of string wrap
  (* > quoted blocks; tok is the '>' *)
  | Blockquote of tok * block list
  | UnorderedList of list_item list
  | OrderedList of list_item list
  (* ---, ***, ___ *)
  | ThematicBreak of tok
  | BlankLine of tok
  [@@deriving show]

(* tok is the marker: -, *, +, or 1. 2. etc. *)
and list_item = ListItem of tok * block list
  [@@deriving show]

and inline =
  | Text of string wrap
  (* `code` *)
  | InlineCode of string wrap bracket
  (* **bold** or __bold__ *)
  | Bold of inline list bracket
  (* *italic* or _italic_ *)
  | Italic of inline list bracket
  (* [text](url) *)
  | Link of inline list bracket (* [...] *) * string wrap bracket (* (...) *)
  (* ![alt](url) *)
  | Image of tok (* '!' *) * string wrap bracket (* [...] *) * string wrap bracket (* (...) *)
  | HardLineBreak of tok
  | SoftLineBreak of tok
  [@@deriving show]

type program = document [@@deriving show]
