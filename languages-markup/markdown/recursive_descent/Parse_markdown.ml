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
module T = Token_markdown

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple recursive-descent parser for Markdown.
 *
 * This parser is intentionally simplified compared to the full CommonMark
 * spec. It handles the most common constructs well enough for
 * syntax highlighting purposes.
 *)

(*****************************************************************************)
(* Tokenizing *)
(*****************************************************************************)

let tokenize_file (file : Fpath.t) : T.token list =
  let ic = open_in (Fpath.to_string file) in
  let lexbuf = Lexing.from_channel ic in
  (* We don't use Parsing_helpers.tokenize_all_and_adjust_pos here because
   * the markdown lexer is line-oriented with state (tracking fenced code
   * blocks), which requires dispatching to different lexer rules depending
   * on context. Instead we drive the lexer manually, but we need to set
   * pos_fname so that Tok.tok_of_lexbuf produces correct file positions. *)
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with
      Lexing.pos_fname = Fpath.to_string file };
  let toks = ref [] in
  let in_fence = ref None in
  (try
    while true do
      let tok = match !in_fence with
        | None ->
          let t = Lexer_markdown.line lexbuf in
          (match t with
           | T.TFenceOpen (fence, _, _) ->
             in_fence := Some fence;
             t
           | _ -> t)
        | Some fence ->
          let t = Lexer_markdown.fenced_code_line fence lexbuf in
          (match t with
           | T.TFenceClose _ ->
             in_fence := None;
             t
           | _ -> t)
      in
      Stack_.push tok toks;
      if T.is_eof tok then raise Exit
    done
  with Exit | End_of_file -> ());
  close_in ic;
  List.rev !toks

(*****************************************************************************)
(* Inline parsing *)
(*****************************************************************************)

(* Parse inline formatting from a string, producing inline AST nodes.
 * This is a simple character-by-character scan.
 * TODO: proper character-level position tracking
 *)
let parse_inlines (s : string) (base_tok : Tok.t) : inline list =
  let len = String.length s in
  if len = 0 then []
  else
    let buf = Buffer.create 64 in
    let result = ref [] in
    let flush () =
      if Buffer.length buf > 0 then begin
        let text = Buffer.contents buf in
        result := Text (text, base_tok) :: !result;
        Buffer.clear buf
      end
    in
    let i = ref 0 in
    while !i < len do
      let c = s.[!i] in
      (match c with
      | '`' ->
        flush ();
        let start = !i in
        let open_tok = Tok.fake_tok base_tok "`" in
        incr i;
        let code_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> '`' do
          Buffer.add_char code_buf s.[!i];
          incr i
        done;
        if !i < len then begin
          let close_tok = Tok.fake_tok base_tok "`" in
          result := InlineCode
            (open_tok, (Buffer.contents code_buf, base_tok), close_tok)
            :: !result;
          incr i
        end else begin
          Buffer.add_char buf '`';
          Buffer.add_string buf (Buffer.contents code_buf);
          i := start + 1 + Buffer.length code_buf
        end
      | '*' when !i + 1 < len && s.[!i + 1] = '*' ->
        flush ();
        let open_tok = Tok.fake_tok base_tok "**" in
        i := !i + 2;
        let inner_buf = Buffer.create 16 in
        while !i + 1 < len && not (s.[!i] = '*' && s.[!i + 1] = '*') do
          Buffer.add_char inner_buf s.[!i];
          incr i
        done;
        if !i + 1 < len then begin
          let close_tok = Tok.fake_tok base_tok "**" in
          let inner = [Text (Buffer.contents inner_buf, base_tok)] in
          result := Bold (open_tok, inner, close_tok) :: !result;
          i := !i + 2
        end else begin
          Buffer.add_string buf "**";
          Buffer.add_string buf (Buffer.contents inner_buf)
        end
      | '*' | '_' ->
        flush ();
        let delim = String.make 1 c in
        let open_tok = Tok.fake_tok base_tok delim in
        incr i;
        let inner_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> c do
          Buffer.add_char inner_buf s.[!i];
          incr i
        done;
        if !i < len then begin
          let close_tok = Tok.fake_tok base_tok delim in
          let inner = [Text (Buffer.contents inner_buf, base_tok)] in
          result := Italic (open_tok, inner, close_tok) :: !result;
          incr i
        end else begin
          Buffer.add_char buf c;
          Buffer.add_string buf (Buffer.contents inner_buf)
        end
      | '[' ->
        flush ();
        let open_bracket = Tok.fake_tok base_tok "[" in
        incr i;
        let text_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> ']' do
          Buffer.add_char text_buf s.[!i];
          incr i
        done;
        if !i < len && !i + 1 < len && s.[!i + 1] = '(' then begin
          let close_bracket = Tok.fake_tok base_tok "]" in
          i := !i + 2;
          let url_buf = Buffer.create 32 in
          while !i < len && s.[!i] <> ')' do
            Buffer.add_char url_buf s.[!i];
            incr i
          done;
          if !i < len then begin
            let open_paren = Tok.fake_tok base_tok "(" in
            let close_paren = Tok.fake_tok base_tok ")" in
            let text_inlines = [Text (Buffer.contents text_buf, base_tok)] in
            let url = Buffer.contents url_buf in
            result := Link
              ((open_bracket, text_inlines, close_bracket),
               (open_paren, (url, base_tok), close_paren))
              :: !result;
            incr i
          end else begin
            Buffer.add_char buf '[';
            Buffer.add_string buf (Buffer.contents text_buf);
            Buffer.add_string buf "](";
            Buffer.add_string buf (Buffer.contents url_buf)
          end
        end else begin
          Buffer.add_char buf '[';
          Buffer.add_string buf (Buffer.contents text_buf);
          if !i < len then (Buffer.add_char buf ']'; incr i)
        end
      | '!' when !i + 1 < len && s.[!i + 1] = '[' ->
        flush ();
        let bang_tok = Tok.fake_tok base_tok "!" in
        i := !i + 2;
        let alt_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> ']' do
          Buffer.add_char alt_buf s.[!i];
          incr i
        done;
        if !i < len && !i + 1 < len && s.[!i + 1] = '(' then begin
          let open_bracket = Tok.fake_tok base_tok "[" in
          let close_bracket = Tok.fake_tok base_tok "]" in
          i := !i + 2;
          let url_buf = Buffer.create 32 in
          while !i < len && s.[!i] <> ')' do
            Buffer.add_char url_buf s.[!i];
            incr i
          done;
          if !i < len then begin
            let open_paren = Tok.fake_tok base_tok "(" in
            let close_paren = Tok.fake_tok base_tok ")" in
            let alt = Buffer.contents alt_buf in
            let url = Buffer.contents url_buf in
            result := Image (bang_tok,
              (open_bracket, (alt, base_tok), close_bracket),
              (open_paren, (url, base_tok), close_paren))
              :: !result;
            incr i
          end else begin
            Buffer.add_string buf "![";
            Buffer.add_string buf (Buffer.contents alt_buf);
            Buffer.add_string buf "](";
            Buffer.add_string buf (Buffer.contents url_buf)
          end
        end else begin
          Buffer.add_string buf "![";
          Buffer.add_string buf (Buffer.contents alt_buf);
          if !i < len then (Buffer.add_char buf ']'; incr i)
        end
      | _ ->
        Buffer.add_char buf c;
        incr i)
    done;
    flush ();
    List.rev !result

(*****************************************************************************)
(* Block parsing *)
(*****************************************************************************)

let rec parse_blocks (toks : T.token list) : document =
  let blocks = ref [] in
  let para_lines = ref [] in

  let flush_paragraph () =
    match !para_lines with
    | [] -> ()
    | lines ->
      let lines = List.rev lines in
      let combined_text = String.concat " "
        (List.map fst lines) in
      let base_tok = snd (List.hd lines) in
      let inlines = parse_inlines combined_text base_tok in
      blocks := Paragraph inlines :: !blocks;
      para_lines := []
  in

  let rec aux = function
    | [] -> flush_paragraph ()
    | T.TEOF _ :: _ -> flush_paragraph ()
    | T.TBlankLine ii :: rest ->
      flush_paragraph ();
      blocks := BlankLine ii :: !blocks;
      aux rest
    | T.THeading (level, text, ii) :: rest ->
      flush_paragraph ();
      let inlines = parse_inlines text ii in
      blocks := Heading (level, ii, inlines) :: !blocks;
      aux rest
    | T.TThematicBreak ii :: rest ->
      flush_paragraph ();
      blocks := ThematicBreak ii :: !blocks;
      aux rest
    | T.TFenceOpen (_, lang, ii) :: rest ->
      flush_paragraph ();
      let code_lines = ref [] in
      let close_tok = ref ii in
      let rec collect = function
        | T.TFenceClose ii :: rest ->
          close_tok := ii;
          rest
        | T.TCodeLine (s, ii) :: rest ->
          code_lines := (s, ii) :: !code_lines;
          collect rest
        | T.TEOF _ :: _ as rest -> rest
        | _ :: rest -> collect rest
        | [] -> []
      in
      let rest = collect rest in
      let code_lines = List.rev !code_lines in
      let code = String.concat "\n" (List.map fst code_lines) in
      let code_tok = match code_lines with
        | (_, ii) :: _ -> ii
        | [] -> ii
      in
      blocks := FencedCode ((lang, ii),
                  (ii, (code, code_tok), !close_tok)) :: !blocks;
      aux rest
    | T.TIndentedCode (s, ii) :: rest ->
      flush_paragraph ();
      blocks := IndentedCode (s, ii) :: !blocks;
      aux rest
    | T.TBlockquoteMarker ii :: rest ->
      flush_paragraph ();
      let inner_toks = ref [] in
      let rec collect_quote = function
        | T.TBlockquoteMarker _ :: rest -> collect_quote rest
        | T.TParagraphLine (s, ii) :: rest ->
          inner_toks := T.TParagraphLine (s, ii) :: !inner_toks;
          (match rest with
           | T.TBlockquoteMarker _ :: _ -> collect_quote rest
           | _ -> rest)
        | rest -> rest
      in
      let rest = collect_quote (T.TParagraphLine ("", ii) :: rest) in
      let inner_blocks = parse_blocks (List.rev !inner_toks) in
      blocks := Blockquote (ii, inner_blocks) :: !blocks;
      aux rest
    | T.TListMarkerUl ii :: rest ->
      flush_paragraph ();
      let items = ref [] in
      let rec collect_items marker rest =
        let item_lines = ref [] in
        let rec item_content = function
          | T.TParagraphLine (s, ii) :: rest ->
            item_lines := (s, ii) :: !item_lines;
            item_content rest
          | rest -> rest
        in
        let rest = item_content rest in
        let item_text = String.concat " "
          (List.rev_map fst !item_lines) in
        let item_tok = match !item_lines with
          | (_, ii) :: _ -> ii
          | [] -> marker
        in
        let inlines = parse_inlines item_text item_tok in
        items := ListItem (marker, [Paragraph inlines]) :: !items;
        match rest with
        | T.TListMarkerUl ii :: rest -> collect_items ii rest
        | _ -> rest
      in
      let rest = collect_items ii rest in
      blocks := UnorderedList (List.rev !items) :: !blocks;
      aux rest
    | T.TListMarkerOl ii :: rest ->
      flush_paragraph ();
      let items = ref [] in
      let rec collect_items marker rest =
        let item_lines = ref [] in
        let rec item_content = function
          | T.TParagraphLine (s, ii) :: rest ->
            item_lines := (s, ii) :: !item_lines;
            item_content rest
          | rest -> rest
        in
        let rest = item_content rest in
        let item_text = String.concat " "
          (List.rev_map fst !item_lines) in
        let item_tok = match !item_lines with
          | (_, ii) :: _ -> ii
          | [] -> marker
        in
        let inlines = parse_inlines item_text item_tok in
        items := ListItem (marker, [Paragraph inlines]) :: !items;
        match rest with
        | T.TListMarkerOl ii :: rest -> collect_items ii rest
        | _ -> rest
      in
      let rest = collect_items ii rest in
      blocks := OrderedList (List.rev !items) :: !blocks;
      aux rest
    | T.TParagraphLine (s, ii) :: rest ->
      para_lines := (s, ii) :: !para_lines;
      aux rest
    | T.TFenceClose _ :: rest ->
      aux rest
    | T.TCodeLine (s, ii) :: rest ->
      para_lines := (s, ii) :: !para_lines;
      aux rest
  in
  aux toks;
  List.rev !blocks

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse (file : Fpath.t) : program * T.token list =
  let toks = tokenize_file file in
  let ast = parse_blocks toks in
  (ast, toks)
