exception SemanticError = Common.SemanticError

let resolve_program prog =
  First_pass.resolve_program prog
  |> Second_pass.resolve_program
  |> Third_pass.resolve_program
  |> Fourth_pass.resolve_program
;;
