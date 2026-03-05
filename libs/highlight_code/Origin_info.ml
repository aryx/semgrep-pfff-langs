type t =
  (* those were extracted from the tree-sitter Concrete Syntax Tree (CST) *)
  | InCST
  (* those are all the ranges in the file that do not correspond to
   * an info in the CST (e.g., space, comments), that is the
   * tokens from the extra: field in tree-sitter grammars
   *)
  | Extra
