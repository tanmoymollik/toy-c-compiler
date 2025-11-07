exception SemanticError = Common.SemanticError

let rec resolve_statement = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If { cnd; thn = resolve_statement thn; els = Option.map resolve_statement els }
  | C_ast.Goto (C_ast.Identifier label) as ret ->
    if Core.exists_label label
    then ret
    else raise (SemanticError ("goto label doesn't exist: " ^ label))
  | C_ast.Label (lbl, stmt) -> C_ast.Label (lbl, resolve_statement stmt)
  | C_ast.Compound block -> C_ast.Compound (resolve_block block)
  | C_ast.While (exp, stmt, iden) -> C_ast.While (exp, resolve_statement stmt, iden)
  | C_ast.DoWhile (stmt, exp, iden) -> C_ast.DoWhile (resolve_statement stmt, exp, iden)
  | C_ast.For { init; cnd; post; body; label } ->
    C_ast.For { init; cnd; post; body = resolve_statement body; label }
  | C_ast.Switch { cnd; body; cases; default; label } ->
    C_ast.Switch { cnd; body = resolve_statement body; cases; default; label }
  | C_ast.Case (exp, stmt, label) -> C_ast.Case (exp, resolve_statement stmt, label)
  | C_ast.Default (stmt, label) -> C_ast.Default (resolve_statement stmt, label)
  | (C_ast.Return _ | C_ast.Expression _ | C_ast.Break _ | C_ast.Continue _ | C_ast.Null)
    as ret -> ret

and resolve_block_item = function
  | C_ast.S s -> C_ast.S (resolve_statement s)
  | C_ast.D _ as ret -> ret

and resolve_block = function
  | C_ast.Block items -> C_ast.Block (List.map resolve_block_item items)
;;

let resolve_function_def = function
  | C_ast.Function { name; body } -> C_ast.Function { name; body = resolve_block body }
;;

let resolve_program = function
  | C_ast.Program f -> C_ast.Program (resolve_function_def f)
;;
