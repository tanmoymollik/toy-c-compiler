let resolve_program prog =
  IdResolver.resolve_program prog
  |> LoopResolver.resolve_program
  |> GotoResolver.resolve_program
  |> TypeChecker.typecheck_program
  |> SwitchResolver.resolve_program
;;
