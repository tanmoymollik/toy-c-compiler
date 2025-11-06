exception SemanticError = Common.SemanticError

let null_label = ""
let make_label prefix = C_ast.Identifier prefix

let rec resolve_statement loop_label = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd
      ; thn = resolve_statement loop_label thn
      ; els = Option.map (resolve_statement loop_label) els
      }
  | C_ast.Label (lbl, stmt) -> C_ast.Label (lbl, resolve_statement loop_label stmt)
  | C_ast.Compound block -> C_ast.Compound (resolve_block loop_label block)
  | C_ast.Break _ ->
    if loop_label = null_label
    then raise (SemanticError "break statement outside of loop");
    C_ast.Break (make_label (Core.break_label loop_label))
  | C_ast.Continue _ ->
    if loop_label = null_label
    then raise (SemanticError "continue statement outside of loop");
    C_ast.Break (make_label (Core.continue_label loop_label))
  | C_ast.While (exp, stmt, _) ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.While (exp, resolve_statement label stmt, make_label label)
  | C_ast.DoWhile (stmt, exp, _) ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.DoWhile (resolve_statement label stmt, exp, make_label label)
  | C_ast.For { init; cnd; post; body; _ } ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.For
      { init; cnd; post; body = resolve_statement label body; label = make_label label }
  | _ as ret -> ret

and resolve_block_item loop_label = function
  | C_ast.S s -> C_ast.S (resolve_statement loop_label s)
  | _ as ret -> ret

and resolve_block loop_label = function
  | C_ast.Block items -> C_ast.Block (List.map (resolve_block_item loop_label) items)
;;

let resolve_function_def = function
  | C_ast.Function { name; body } ->
    C_ast.Function { name; body = resolve_block null_label body }
;;

let resolve_program = function
  | C_ast.Program f -> C_ast.Program (resolve_function_def f)
;;
