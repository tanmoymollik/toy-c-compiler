exception SemanticError = Common.SemanticError

let rec resolve_statement = function
  | C_ast.Goto (C_ast.Identifier label) as ret ->
    if State.exists_label label
    then ret
    else raise (SemanticError ("goto label doesn't exist: " ^ label))
  | C_ast.Label (lbl, stmt) -> C_ast.Label (lbl, resolve_statement stmt)
  | C_ast.Compound block -> C_ast.Compound (resolve_block block)
  | _ as ret -> ret

and resolve_block_item = function
  | C_ast.S s -> C_ast.S (resolve_statement s)
  | _ as ret -> ret

and resolve_block = function
  | C_ast.Block items -> C_ast.Block (List.map resolve_block_item items)
;;

let resolve_function_def = function
  | C_ast.Function { name; body } -> C_ast.Function { name; body = resolve_block body }
;;

let resolve_program = function
  | C_ast.Program f -> C_ast.Program (resolve_function_def f)
;;
