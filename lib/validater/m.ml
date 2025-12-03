let resolve_program prog =
  Id_resolver.resolve_program prog
  |> Loop_resolver.resolve_program
  |> Goto_resolver.resolve_program
  |> Type_checker.typecheck_program
  |> Switch_resolver.resolve_program
;;
