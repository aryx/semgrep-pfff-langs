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
open Fpath_.Operators
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

(* claude: The lexer is line-oriented with state (fenced code blocks),
 * so we drive it manually rather than passing a single tokenizer function
 * to Parsing_helpers.tokenize_all_and_adjust_pos. We use
 * Pos.full_converters_large to fill in line/col after tokenizing. *)
let tokenize_file (file : Fpath.t) : T.token list =
  let toks = ref [] in
  UFile.with_open_in file (fun ic ->
    let lexbuf = Lexing.from_channel ic in
    lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with
        Lexing.pos_fname = Fpath.to_string file };
    let in_fence = ref None in
    (try
      while true do
        (* claude: drain any pending tokens queued by the lexer
         * (e.g., TFenceLang emitted alongside TFenceOpen) *)
        while not (Queue.is_empty Lexer_markdown._pending) do
          Stack_.push (Queue.pop Lexer_markdown._pending) toks
        done;
        let tok = match !in_fence with
          | None ->
            let t = Lexer_markdown.line lexbuf in
            (match t with
             | T.TFenceOpen (fence, _) ->
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
    (* claude: drain any remaining pending tokens *)
    while not (Queue.is_empty Lexer_markdown._pending) do
      Stack_.push (Queue.pop Lexer_markdown._pending) toks
    done);
  let toks = List.rev !toks in
  (* claude: fill in line/col from bytepos using the file's line table *)
  let table = Pos.full_converters_large file in
  toks |> List.map (T.visitor_info_of_tok (fun tok ->
    Tok.fix_location (Tok.complete_location file table) tok))

(*****************************************************************************)
(* Inline parsing *)
(*****************************************************************************)

(* claude: Create an OriginTok at a byte offset within a line.
 * base_tok must be an OriginTok pointing to the start of the text;
 * offset is the character position within the text string. *)
let mk_tok (base_tok : Tok.t) (offset : int) (s : string) : Tok.t =
  let base_loc = Tok.unsafe_loc_of_tok base_tok in
  Tok.OriginTok {
    Loc.str = s;
    pos = {
      Pos.bytepos = base_loc.pos.bytepos + offset;
      line = base_loc.pos.line;
      column = base_loc.pos.column + offset;
      file = base_loc.pos.file;
    }
  }

(* Parse inline formatting from a string, producing inline AST nodes.
 * Uses mk_tok to create real OriginTok values at correct byte positions. *)
let parse_inlines (s : string) (base_tok : Tok.t) : inline list =
  let len = String.length s in
  if len = 0 then []
  else
    let buf = Buffer.create 64 in
    let text_start = ref 0 in
    let result = ref [] in
    let flush () =
      if Buffer.length buf > 0 then begin
        let text = Buffer.contents buf in
        let tok = mk_tok base_tok !text_start text in
        result := Text (text, tok) :: !result;
        Buffer.clear buf
      end
    in
    let i = ref 0 in
    while !i < len do
      let c = s.[!i] in
      (match c with
      | '`' ->
        flush ();
        let open_tok = mk_tok base_tok !i "`" in
        incr i;
        let code_start = !i in
        let code_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> '`' do
          Buffer.add_char code_buf s.[!i];
          incr i
        done;
        if !i < len then begin
          let close_tok = mk_tok base_tok !i "`" in
          let code_s = Buffer.contents code_buf in
          let code_tok = mk_tok base_tok code_start code_s in
          result := InlineCode
            (open_tok, (code_s, code_tok), close_tok)
            :: !result;
          incr i;
          text_start := !i
        end else begin
          (* claude: no closing backtick, treat as plain text *)
          i := code_start - 1;
          Buffer.add_char buf '`';
          incr i;
          text_start := code_start - 1
        end
      | '*' when !i + 1 < len && s.[!i + 1] = '*' ->
        flush ();
        let open_tok = mk_tok base_tok !i "**" in
        i := !i + 2;
        let inner_start = !i in
        let inner_buf = Buffer.create 16 in
        while !i + 1 < len && not (s.[!i] = '*' && s.[!i + 1] = '*') do
          Buffer.add_char inner_buf s.[!i];
          incr i
        done;
        if !i + 1 < len then begin
          let close_tok = mk_tok base_tok !i "**" in
          let inner_s = Buffer.contents inner_buf in
          let inner_tok = mk_tok base_tok inner_start inner_s in
          let inner = [Text (inner_s, inner_tok)] in
          result := Bold (open_tok, inner, close_tok) :: !result;
          i := !i + 2;
          text_start := !i
        end else begin
          Buffer.add_string buf "**";
          Buffer.add_string buf (Buffer.contents inner_buf);
          text_start := !i
        end
      | '*' | '_' ->
        flush ();
        let delim = String.make 1 c in
        let open_tok = mk_tok base_tok !i delim in
        incr i;
        let inner_start = !i in
        let inner_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> c do
          Buffer.add_char inner_buf s.[!i];
          incr i
        done;
        if !i < len then begin
          let close_tok = mk_tok base_tok !i delim in
          let inner_s = Buffer.contents inner_buf in
          let inner_tok = mk_tok base_tok inner_start inner_s in
          let inner = [Text (inner_s, inner_tok)] in
          result := Italic (open_tok, inner, close_tok) :: !result;
          incr i;
          text_start := !i
        end else begin
          Buffer.add_char buf c;
          Buffer.add_string buf (Buffer.contents inner_buf);
          text_start := !i
        end
      | '[' ->
        flush ();
        let open_bracket = mk_tok base_tok !i "[" in
        incr i;
        let text_inner_start = !i in
        let text_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> ']' do
          Buffer.add_char text_buf s.[!i];
          incr i
        done;
        if !i < len && !i + 1 < len && s.[!i + 1] = '(' then begin
          let close_bracket = mk_tok base_tok !i "]" in
          incr i;
          let open_paren = mk_tok base_tok !i "(" in
          incr i;
          let url_start = !i in
          let url_buf = Buffer.create 32 in
          while !i < len && s.[!i] <> ')' do
            Buffer.add_char url_buf s.[!i];
            incr i
          done;
          if !i < len then begin
            let close_paren = mk_tok base_tok !i ")" in
            let text_s = Buffer.contents text_buf in
            let text_tok = mk_tok base_tok text_inner_start text_s in
            let text_inlines = [Text (text_s, text_tok)] in
            let url = Buffer.contents url_buf in
            let url_tok = mk_tok base_tok url_start url in
            result := Link
              ((open_bracket, text_inlines, close_bracket),
               (open_paren, (url, url_tok), close_paren))
              :: !result;
            incr i;
            text_start := !i
          end else begin
            Buffer.add_char buf '[';
            Buffer.add_string buf (Buffer.contents text_buf);
            Buffer.add_string buf "](";
            Buffer.add_string buf (Buffer.contents url_buf);
            text_start := !i
          end
        end else begin
          Buffer.add_char buf '[';
          Buffer.add_string buf (Buffer.contents text_buf);
          if !i < len then (Buffer.add_char buf ']'; incr i);
          text_start := !i
        end
      | '!' when !i + 1 < len && s.[!i + 1] = '[' ->
        flush ();
        let bang_tok = mk_tok base_tok !i "!" in
        incr i;
        let open_bracket = mk_tok base_tok !i "[" in
        incr i;
        let alt_start = !i in
        let alt_buf = Buffer.create 16 in
        while !i < len && s.[!i] <> ']' do
          Buffer.add_char alt_buf s.[!i];
          incr i
        done;
        if !i < len && !i + 1 < len && s.[!i + 1] = '(' then begin
          let close_bracket = mk_tok base_tok !i "]" in
          incr i;
          let open_paren = mk_tok base_tok !i "(" in
          incr i;
          let url_start = !i in
          let url_buf = Buffer.create 32 in
          while !i < len && s.[!i] <> ')' do
            Buffer.add_char url_buf s.[!i];
            incr i
          done;
          if !i < len then begin
            let close_paren = mk_tok base_tok !i ")" in
            let alt = Buffer.contents alt_buf in
            let alt_tok = mk_tok base_tok alt_start alt in
            let url = Buffer.contents url_buf in
            let url_tok = mk_tok base_tok url_start url in
            result := Image (bang_tok,
              (open_bracket, (alt, alt_tok), close_bracket),
              (open_paren, (url, url_tok), close_paren))
              :: !result;
            incr i;
            text_start := !i
          end else begin
            Buffer.add_string buf "![";
            Buffer.add_string buf (Buffer.contents alt_buf);
            Buffer.add_string buf "](";
            Buffer.add_string buf (Buffer.contents url_buf);
            text_start := !i
          end
        end else begin
          Buffer.add_string buf "![";
          Buffer.add_string buf (Buffer.contents alt_buf);
          if !i < len then (Buffer.add_char buf ']'; incr i);
          text_start := !i
        end
      | _ ->
        if Buffer.length buf = 0 then text_start := !i;
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
    | T.THeading (level, marker_ii, text, text_ii) :: rest ->
      flush_paragraph ();
      let inlines = parse_inlines text text_ii in
      blocks := Heading (level, marker_ii, inlines) :: !blocks;
      aux rest
    | T.TThematicBreak ii :: rest ->
      flush_paragraph ();
      blocks := ThematicBreak ii :: !blocks;
      aux rest
    | T.TFenceOpen (_, ii) :: rest ->
      flush_paragraph ();
      let lang = ref None in
      let code_lines = ref [] in
      let close_tok = ref ii in
      let rec collect = function
        | T.TFenceLang (s, ii) :: rest ->
          lang := Some (Some s, ii);
          collect rest
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
      let code_lines_rev = List.rev !code_lines in
      let code = String.concat "\n" (List.map fst code_lines_rev) in
      let code_tok = match code_lines_rev with
        | (_, ii) :: _ -> ii
        | [] -> ii
      in
      let lang_wrap = match !lang with
        | Some (Some s, ii) -> (Some s, ii)
        | _ -> (None, ii)
      in
      blocks := FencedCode (lang_wrap,
                  (ii, (code, code_tok), !close_tok)) :: !blocks;
      aux rest
    | T.TFenceLang _ :: rest ->
      (* claude: stray TFenceLang outside fenced block, skip *)
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
    | T.TInlineTok _ :: rest ->
      (* claude: skip; these are only in the token list, not the parse stream *)
      aux rest
  in
  aux toks;
  List.rev !blocks

(*****************************************************************************)
(* Collect tokens from the AST *)
(*****************************************************************************)

(* claude: Walk inline nodes and collect all Tok.t values. *)
let collect_inline_toks (inlines : inline list) : Tok.t list =
  let acc = ref [] in
  let add tok = acc := tok :: !acc in
  let rec visit = function
    | Text (_, tok) -> add tok
    | InlineCode (open_tok, (_, code_tok), close_tok) ->
      add open_tok; add code_tok; add close_tok
    | Bold (open_tok, inner, close_tok) ->
      add open_tok; List.iter visit inner; add close_tok
    | Italic (open_tok, inner, close_tok) ->
      add open_tok; List.iter visit inner; add close_tok
    | Link ((ob, text_inner, cb), (op, (_, url_tok), cp)) ->
      add ob; List.iter visit text_inner; add cb;
      add op; add url_tok; add cp
    | Image (bang, (ob, (_, alt_tok), cb), (op, (_, url_tok), cp)) ->
      add bang; add ob; add alt_tok; add cb;
      add op; add url_tok; add cp
    | HardLineBreak tok | SoftLineBreak tok -> add tok
  in
  List.iter visit inlines;
  List.rev !acc

(* claude: Collect all Tok.t from every block in the AST.
 * For blocks with inline content, returns the inline-level toks.
 * For other blocks, returns the block-level tok. *)
let collect_ast_toks (ast : program) : Tok.t list =
  let acc = ref [] in
  let add tok = acc := tok :: !acc in
  let rec visit_block = function
    | Heading (_, marker_ii, inlines) ->
      add marker_ii;
      List.iter add (collect_inline_toks inlines)
    | Paragraph inlines ->
      List.iter add (collect_inline_toks inlines)
    | FencedCode ((_, lang_tok), (open_tok, (_, code_tok), close_tok)) ->
      add open_tok; add lang_tok; add code_tok; add close_tok
    | IndentedCode (_, ii) -> add ii
    | Blockquote (ii, blocks) ->
      add ii; List.iter visit_block blocks
    | UnorderedList items | OrderedList items ->
      items |> List.iter (fun (ListItem (ii, blocks)) ->
        add ii; List.iter visit_block blocks)
    | ThematicBreak ii -> add ii
    | BlankLine ii -> add ii
  in
  List.iter visit_block ast;
  List.rev !acc

(*****************************************************************************)
(* Gap filling *)
(*****************************************************************************)

(* claude: Same approach as Parse_languages.add_extra_infos: walk sorted
 * Tok.t values by byte position and create filler tokens (spaces, newlines)
 * for any uncovered byte ranges. This ensures 100% file coverage. *)
let fill_gaps (file : Fpath.t) (toks : Tok.t list) : T.token list =
  let bigstr = UFile.Legacy.read_file !!file in
  let max = String.length bigstr in
  let conv = Pos.full_converters_large file in
  let mk_filler current end_pos =
    let (line, column) = conv.bytepos_to_linecol_fun current in
    let str = String.sub bigstr current (end_pos - current) in
    let loc : Loc.t = {
      pos = { Pos.file; line; column; bytepos = current }; str } in
    T.TInlineTok (str, Tok.tok_of_loc loc)
  in
  let rec aux current = function
    | [] ->
      if current < max then [mk_filler current max]
      else []
    | x :: xs ->
      if Tok.is_fake x then aux current xs
      else
        let loc = Tok.unsafe_loc_of_tok x in
        let tok_start = loc.pos.bytepos in
        let tok_end = tok_start + String.length loc.str in
        if current < tok_start then
          (* claude: gap before this token — fill it *)
          mk_filler current tok_start
          :: T.TInlineTok (loc.str, x) :: aux tok_end xs
        else if current = tok_start then
          T.TInlineTok (loc.str, x) :: aux tok_end xs
        else
          (* claude: overlap — skip, can happen with heading marker/text *)
          aux (Int.max current tok_end) xs
  in
  aux 0 toks

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse (file : Fpath.t) : (program, T.token) Parsing_result.t =
  let toks = tokenize_file file in
  let ast = parse_blocks toks in
  (* claude: Collect fine-grained toks from the AST (inline-level for
   * paragraphs/headings, block-level for everything else), sort by
   * byte position, then fill gaps for spaces/newlines. *)
  let ast_toks = collect_ast_toks ast in
  let sorted = List.sort (fun a b ->
    compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b)) ast_toks in
  let all_toks = fill_gaps file sorted in
  { Parsing_result.ast; tokens = all_toks; stat = Parsing_stat.correct_stat file }
