exception SemanticError = Semantic_error.SemanticError

let resolve_program prog =
  Resolve_var.resolve_program prog |> Resolve_goto.resolve_program
;;
