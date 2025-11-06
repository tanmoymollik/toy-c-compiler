exception SemanticError = Common.SemanticError

let resolve_program prog =
  Resolve_var.resolve_program prog |> Resolve_goto.resolve_program
;;
