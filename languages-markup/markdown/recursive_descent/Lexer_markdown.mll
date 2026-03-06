{
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
(* A line-oriented lexer for Markdown.
 *
 * Markdown is fundamentally line-oriented: block structure is determined
 * by line prefixes (headings, list markers, fences, etc.) and inline
 * formatting happens within lines. This lexer handles the block-level
 * tokenization; inline parsing is done in a second pass by the parser
 * using real OriginTok values computed from byte offsets.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf

(* claude: Queue used to emit multiple tokens from a single lexer rule.
 * The main driver in Parse_markdown.tokenize_file drains this queue
 * before calling the lexer again. *)
let _pending : Token_markdown.token Queue.t = Queue.create ()

}

(*****************************************************************************)
(* Regexps *)
(*****************************************************************************)

let newline = '\n' | "\r\n" | '\r'
let space = [' ' '\t']

(*****************************************************************************)
(* Token type *)
(*****************************************************************************)

(* See Token_markdown.ml for the token type definition *)

(*****************************************************************************)
(* Rules *)
(*****************************************************************************)

(* Main line-oriented lexer: processes one line at a time *)
rule line = parse
  (* ATX headings: capture marker tok before consuming rest of line *)
  | (('#' '#'* as hashes) ' ')
      { let marker_ii = tokinfo lexbuf in
        let rest = rest_of_line lexbuf in
        let text_ii = tokinfo lexbuf in
        Token_markdown.THeading (String.length hashes, marker_ii, rest, text_ii) }

  (* Fenced code block opening: ``` or ~~~ with optional language.
   * We emit the fence token and queue a TFenceLang if present. *)
  | ("```" '`'* as fence)
      { let fence_ii = tokinfo lexbuf in
        let lang_str = rest_of_line lexbuf in
        let lang_trimmed = String.trim lang_str in
        if lang_trimmed <> "" then begin
          let lang_ii = tokinfo lexbuf in
          Queue.push (Token_markdown.TFenceLang (lang_trimmed, lang_ii)) _pending
        end;
        Token_markdown.TFenceOpen (fence, fence_ii) }
  | ("~~~" '~'* as fence)
      { let fence_ii = tokinfo lexbuf in
        let lang_str = rest_of_line lexbuf in
        let lang_trimmed = String.trim lang_str in
        if lang_trimmed <> "" then begin
          let lang_ii = tokinfo lexbuf in
          Queue.push (Token_markdown.TFenceLang (lang_trimmed, lang_ii)) _pending
        end;
        Token_markdown.TFenceOpen (fence, fence_ii) }

  (* Thematic break: ---, ***, ___ (with optional spaces) *)
  | space* ('-' space* '-' space* '-' (space | '-')*)
  | space* ('*' space* '*' space* '*' (space | '*')*)
  | space* ('_' space* '_' space* '_' (space | '_')*)
      { Token_markdown.TThematicBreak (tokinfo lexbuf) }

  (* Blockquote *)
  | ('>' ' '?)
      { Token_markdown.TBlockquoteMarker (tokinfo lexbuf) }

  (* Unordered list markers *)
  | (space* (['*' '+' '-'] as _marker) ' ')
      { Token_markdown.TListMarkerUl (tokinfo lexbuf) }

  (* Ordered list markers *)
  | (space* ['0'-'9']+ ['.'] ' ')
      { Token_markdown.TListMarkerOl (tokinfo lexbuf) }

  (* Indented code block (4 spaces or 1 tab) *)
  | "    "
      { let rest = rest_of_line lexbuf in
        Token_markdown.TIndentedCode (rest, tokinfo lexbuf) }
  | '\t'
      { let rest = rest_of_line lexbuf in
        Token_markdown.TIndentedCode (rest, tokinfo lexbuf) }

  (* Blank line *)
  | space* newline
      { Token_markdown.TBlankLine (tokinfo lexbuf) }
  | space* eof
      { Token_markdown.TEOF (tokinfo lexbuf) }

  (* Any other line is a paragraph line *)
  | ""
      { let content = rest_of_line lexbuf in
        Token_markdown.TParagraphLine (content, tokinfo lexbuf) }

(* Consume the rest of a line *)
and rest_of_line = parse
  | ([^ '\n' '\r']* as s) (newline | eof)  { s }
  | ([^ '\n' '\r']* as s)                  { s }

(* Inside a fenced code block *)
and fenced_code_line fence = parse
  | space* (("```" '`'* | "~~~" '~'*) as close) space* (newline | eof)
      { if String.length close >= String.length fence
           && close.[0] = fence.[0]
        then Token_markdown.TFenceClose (tokinfo lexbuf)
        else Token_markdown.TCodeLine (tok lexbuf, tokinfo lexbuf) }
  | ([^ '\n' '\r']* as s) (newline | eof)
      { Token_markdown.TCodeLine (s, tokinfo lexbuf) }
  | eof
      { Token_markdown.TEOF (tokinfo lexbuf) }
