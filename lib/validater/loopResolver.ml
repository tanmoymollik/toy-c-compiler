open Common
open C_ast

exception SemanticError = Errors.SemanticError

let null_label = ""
let make_label prefix = Identifier prefix

let rec resolve_statement loop_lbl switch_lbl inner_loop = function
  | If { cnd; thn; els } ->
    If
      { cnd
      ; thn = resolve_statement loop_lbl switch_lbl inner_loop thn
      ; els = Option.map (resolve_statement loop_lbl switch_lbl inner_loop) els
      }
  | Label (lbl, stmt) -> Label (lbl, resolve_statement loop_lbl switch_lbl inner_loop stmt)
  | Compound block -> Compound (resolve_block loop_lbl switch_lbl inner_loop block)
  | Break _ ->
    let label = if inner_loop then loop_lbl else switch_lbl in
    if label = null_label
    then raise (SemanticError "break statement outside of loop or switch");
    Break (make_label label)
  | Continue _ ->
    if loop_lbl = null_label
    then raise (SemanticError "continue statement outside of loop");
    Continue (make_label loop_lbl)
  | While (exp, stmt, _) ->
    let label = Core.make_unique_label Core.loop_label in
    While (exp, resolve_statement label switch_lbl true stmt, make_label label)
  | DoWhile (stmt, exp, _) ->
    let label = Core.make_unique_label Core.loop_label in
    DoWhile (resolve_statement label switch_lbl true stmt, exp, make_label label)
  | For { init; cnd; post; body; _ } ->
    let label = Core.make_unique_label Core.loop_label in
    For
      { init
      ; cnd
      ; post
      ; body = resolve_statement label switch_lbl true body
      ; label = make_label label
      }
  | Switch { cnd; body; cases; default; _ } ->
    let label = Core.make_unique_label Core.switch_label in
    let body = resolve_statement loop_lbl label false body in
    Switch { cnd; body; cases; default; label = make_label label }
  | Case (exp, stmt, _) ->
    if switch_lbl = null_label
    then raise (SemanticError "case statement outside of switch");
    Case
      (exp, resolve_statement loop_lbl switch_lbl inner_loop stmt, make_label switch_lbl)
  | Default (stmt, _) ->
    if switch_lbl = null_label
    then raise (SemanticError "default statement outside of switch");
    Default (resolve_statement loop_lbl switch_lbl inner_loop stmt, make_label switch_lbl)
  | (Return _ | Expression _ | Goto _ | Null) as ret -> ret

and resolve_block_item loop_lbl switch_lbl inner_loop = function
  | S s -> S (resolve_statement loop_lbl switch_lbl inner_loop s)
  | D _ as ret -> ret

and resolve_block loop_lbl switch_lbl inner_loop = function
  | Block items ->
    Block (List.map (resolve_block_item loop_lbl switch_lbl inner_loop) items)
;;

let resolve_function_decl = function
  | { name; params; body; ftp; storage } ->
    let body = Option.map (resolve_block null_label null_label false) body in
    { name; params; body; ftp; storage }
;;

let resolve_declaration = function
  | FunDecl f -> FunDecl (resolve_function_decl f)
  | (VarDecl _ | StructDecl _) as ret -> ret
;;

let resolve_program = function
  | Program dns -> Program (List.map resolve_declaration dns)
;;
