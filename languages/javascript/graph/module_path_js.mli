val resolve_path :
  root:Common2.filename (* where to find node_modules *) ->
  pwd:Common2.filename (* pwd of importer *) ->
  string ->
  (* full path *)
  Common2.filename option
