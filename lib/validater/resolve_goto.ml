exception SemanticError = Semantic_error.SemanticError

let resolve_statement = function
  | C_ast.Goto (C_ast.Identifier label) as ret ->
    if State.exists_label label
    then ret
    else raise (SemanticError ("goto label doesn't exist: " ^ label))
  | _ as ret -> ret
;;

let resolve_block_item = function
  | C_ast.S s -> C_ast.S (resolve_statement s)
  | _ as ret -> ret
;;

let resolve_function_def = function
  | C_ast.Function { name; body } ->
    C_ast.Function { name; body = List.map resolve_block_item body }
;;

let resolve_program = function
  | C_ast.Program f -> C_ast.Program (resolve_function_def f)
;;
