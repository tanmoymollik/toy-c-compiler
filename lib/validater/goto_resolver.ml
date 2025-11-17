exception SemanticError = Common.SemanticError

let label_map : (string, bool) Hashtbl.t = Hashtbl.create 100

let exists_label label =
  match Hashtbl.find_opt label_map label with
  | Some _ -> true
  | None -> false
;;

let add_label label = Hashtbl.add label_map label true

let rec resolve_statement fun_name = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd
      ; thn = resolve_statement fun_name thn
      ; els = Option.map (resolve_statement fun_name) els
      }
  | C_ast.Goto (C_ast.Identifier label) as ret ->
    (match fun_name with
     | Some fun_name ->
       let label = Core.goto_label label fun_name in
       C_ast.Goto (C_ast.Identifier label)
     | None ->
       if not (exists_label label)
       then raise (SemanticError ("goto label doesn't exist: " ^ label));
       ret)
  | C_ast.Label (C_ast.Identifier label, stmt) as ret ->
    (match fun_name with
     | Some f_name ->
       let label = Core.goto_label label f_name in
       if exists_label label then raise (SemanticError ("Duplicate label: " ^ label));
       add_label label;
       C_ast.Label (C_ast.Identifier label, resolve_statement fun_name stmt)
     | None -> ret)
  | C_ast.Compound block -> C_ast.Compound (resolve_block fun_name block)
  | C_ast.While (exp, stmt, iden) ->
    C_ast.While (exp, resolve_statement fun_name stmt, iden)
  | C_ast.DoWhile (stmt, exp, iden) ->
    C_ast.DoWhile (resolve_statement fun_name stmt, exp, iden)
  | C_ast.For { init; cnd; post; body; label } ->
    C_ast.For { init; cnd; post; body = resolve_statement fun_name body; label }
  | C_ast.Switch { cnd; body; cases; default; label } ->
    C_ast.Switch { cnd; body = resolve_statement fun_name body; cases; default; label }
  | C_ast.Case (exp, stmt, label) ->
    C_ast.Case (exp, resolve_statement fun_name stmt, label)
  | C_ast.Default (stmt, label) -> C_ast.Default (resolve_statement fun_name stmt, label)
  | (C_ast.Return _ | C_ast.Expression _ | C_ast.Break _ | C_ast.Continue _ | C_ast.Null)
    as ret -> ret

and resolve_block_item fun_name = function
  | C_ast.S s -> C_ast.S (resolve_statement fun_name s)
  | C_ast.D _ as ret -> ret

and resolve_block fun_name = function
  | C_ast.Block items -> C_ast.Block (List.map (resolve_block_item fun_name) items)
;;

let resolve_function_decl = function
  | C_ast.{ name = C_ast.Identifier iden; params; body; storage } ->
    let first_pass = Option.map (resolve_block (Some iden)) body in
    let body = Option.map (resolve_block None) first_pass in
    C_ast.{ name = C_ast.Identifier iden; params; body; storage }
;;

let resolve_declaration = function
  | C_ast.FunDecl f -> C_ast.FunDecl (resolve_function_decl f)
  | C_ast.VarDecl _ as ret -> ret
;;

let resolve_program = function
  | C_ast.Program dns -> C_ast.Program (List.map resolve_declaration dns)
;;
