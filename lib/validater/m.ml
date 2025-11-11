exception SemanticError = Common.SemanticError

let resolve_program prog =
  Id_resolver.resolve_program prog
  |> Type_checker.typecheck_program
  |> Loop_resolver.resolve_program
  |> Goto_resolver.resolve_program
  |> Switch_resolver.resolve_program
;;
