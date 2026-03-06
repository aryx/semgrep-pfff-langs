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
(* Token type *)
(*****************************************************************************)

type token =
  (* Block-level tokens *)
  | THeading of int (* level 1-6 *) * string (* rest of line *) * Tok.t
  | TFenceOpen of string (* fence chars *) * string option (* lang *) * Tok.t
  | TFenceClose of Tok.t
  | TCodeLine of string * Tok.t
  | TThematicBreak of Tok.t
  | TBlockquoteMarker of Tok.t
  | TListMarkerUl of Tok.t
  | TListMarkerOl of Tok.t
  | TIndentedCode of string * Tok.t
  | TParagraphLine of string * Tok.t
  | TBlankLine of Tok.t

  | TEOF of Tok.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let info_of_tok = function
  | THeading (_, _, ii) -> ii
  | TFenceOpen (_, _, ii) -> ii
  | TFenceClose ii -> ii
  | TCodeLine (_, ii) -> ii
  | TThematicBreak ii -> ii
  | TBlockquoteMarker ii -> ii
  | TListMarkerUl ii -> ii
  | TListMarkerOl ii -> ii
  | TIndentedCode (_, ii) -> ii
  | TParagraphLine (_, ii) -> ii
  | TBlankLine ii -> ii
  | TEOF ii -> ii

let visitor_info_of_tok f = function
  | THeading (n, s, ii) -> THeading (n, s, f ii)
  | TFenceOpen (fence, lang, ii) -> TFenceOpen (fence, lang, f ii)
  | TFenceClose ii -> TFenceClose (f ii)
  | TCodeLine (s, ii) -> TCodeLine (s, f ii)
  | TThematicBreak ii -> TThematicBreak (f ii)
  | TBlockquoteMarker ii -> TBlockquoteMarker (f ii)
  | TListMarkerUl ii -> TListMarkerUl (f ii)
  | TListMarkerOl ii -> TListMarkerOl (f ii)
  | TIndentedCode (s, ii) -> TIndentedCode (s, f ii)
  | TParagraphLine (s, ii) -> TParagraphLine (s, f ii)
  | TBlankLine ii -> TBlankLine (f ii)
  | TEOF ii -> TEOF (f ii)

let is_eof = function
  | TEOF _ -> true
  | _ -> false
