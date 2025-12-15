exception SemanticError = Errors.SemanticError

let null_label = ""
let make_label prefix = Common.Identifier prefix

let rec resolve_statement loop_lbl switch_lbl inner_loop = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd
      ; thn = resolve_statement loop_lbl switch_lbl inner_loop thn
      ; els = Option.map (resolve_statement loop_lbl switch_lbl inner_loop) els
      }
  | C_ast.Label (lbl, stmt) ->
    C_ast.Label (lbl, resolve_statement loop_lbl switch_lbl inner_loop stmt)
  | C_ast.Compound block ->
    C_ast.Compound (resolve_block loop_lbl switch_lbl inner_loop block)
  | C_ast.Break _ ->
    let label = if inner_loop then loop_lbl else switch_lbl in
    if label = null_label
    then raise (SemanticError "break statement outside of loop or switch");
    C_ast.Break (make_label label)
  | C_ast.Continue _ ->
    if loop_lbl = null_label
    then raise (SemanticError "continue statement outside of loop");
    C_ast.Continue (make_label loop_lbl)
  | C_ast.While (exp, stmt, _) ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.While (exp, resolve_statement label switch_lbl true stmt, make_label label)
  | C_ast.DoWhile (stmt, exp, _) ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.DoWhile (resolve_statement label switch_lbl true stmt, exp, make_label label)
  | C_ast.For { init; cnd; post; body; _ } ->
    let label = Core.make_unique_label Core.loop_label in
    C_ast.For
      { init
      ; cnd
      ; post
      ; body = resolve_statement label switch_lbl true body
      ; label = make_label label
      }
  | C_ast.Switch { cnd; body; cases; default; _ } ->
    let label = Core.make_unique_label Core.switch_label in
    let body = resolve_statement loop_lbl label false body in
    C_ast.Switch { cnd; body; cases; default; label = make_label label }
  | C_ast.Case (exp, stmt, _) ->
    if switch_lbl = null_label
    then raise (SemanticError "case statement outside of switch");
    C_ast.Case
      (exp, resolve_statement loop_lbl switch_lbl inner_loop stmt, make_label switch_lbl)
  | C_ast.Default (stmt, _) ->
    if switch_lbl = null_label
    then raise (SemanticError "default statement outside of switch");
    C_ast.Default
      (resolve_statement loop_lbl switch_lbl inner_loop stmt, make_label switch_lbl)
  | (C_ast.Return _ | C_ast.Expression _ | C_ast.Goto _ | C_ast.Null) as ret -> ret

and resolve_block_item loop_lbl switch_lbl inner_loop = function
  | C_ast.S s -> C_ast.S (resolve_statement loop_lbl switch_lbl inner_loop s)
  | C_ast.D _ as ret -> ret

and resolve_block loop_lbl switch_lbl inner_loop = function
  | C_ast.Block items ->
    C_ast.Block (List.map (resolve_block_item loop_lbl switch_lbl inner_loop) items)
;;

let resolve_function_decl = function
  | C_ast.{ name; params; body; ftp; storage } ->
    let body = Option.map (resolve_block null_label null_label false) body in
    C_ast.{ name; params; body; ftp; storage }
;;

let resolve_declaration = function
  | C_ast.FunDecl f -> C_ast.FunDecl (resolve_function_decl f)
  | C_ast.VarDecl _ as ret -> ret
;;

let resolve_program = function
  | C_ast.Program dns -> C_ast.Program (List.map resolve_declaration dns)
;;
