val visit_program :
  tag_hook:(Tok.t -> Highlight_code.category -> unit) ->
  Highlight_code.highlighter_preferences ->
  Fpath.t ->
  AST_markdown.program * Token_markdown.token list ->
  unit
