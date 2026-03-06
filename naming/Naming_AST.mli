(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

(* pro-scan hook *)
val pro_hook_normalize_ast_generic_type :
  (Lang.t -> AST_generic.type_ -> AST_generic.type_) option Hook.t

(* Hook for type inference, set by osemgrep's Typing module.
 * Combines the old Typing.hook_type_of_expr + Type.to_ast_generic_type_. *)
val hook_type_of_expr :
  (Lang.t -> AST_generic.expr -> AST_generic.type_ option) option ref
