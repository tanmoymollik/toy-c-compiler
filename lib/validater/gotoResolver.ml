open Common
open C_ast

exception SemanticError = Errors.SemanticError

let label_map : (string, bool) Hashtbl.t = Hashtbl.create 100

let exists_label label =
  match Hashtbl.find_opt label_map label with
  | Some _ -> true
  | None -> false
;;

let add_label label = Hashtbl.add label_map label true

let rec resolve_statement fun_name = function
  | If { cnd; thn; els } ->
    If
      { cnd
      ; thn = resolve_statement fun_name thn
      ; els = Option.map (resolve_statement fun_name) els
      }
  | Goto (Identifier label) as ret ->
    (match fun_name with
     | Some fun_name ->
       let label = Core.goto_label label fun_name in
       Goto (Identifier label)
     | None ->
       if not (exists_label label)
       then raise (SemanticError ("goto label doesn't exist: " ^ label));
       ret)
  | Label (Identifier label, stmt) as ret ->
    (match fun_name with
     | Some f_name ->
       let label = Core.goto_label label f_name in
       if exists_label label then raise (SemanticError ("Duplicate label: " ^ label));
       add_label label;
       Label (Identifier label, resolve_statement fun_name stmt)
     | None -> ret)
  | Compound block -> Compound (resolve_block fun_name block)
  | While (exp, stmt, iden) -> While (exp, resolve_statement fun_name stmt, iden)
  | DoWhile (stmt, exp, iden) -> DoWhile (resolve_statement fun_name stmt, exp, iden)
  | For { init; cnd; post; body; label } ->
    For { init; cnd; post; body = resolve_statement fun_name body; label }
  | Switch { cnd; body; cases; default; label } ->
    Switch { cnd; body = resolve_statement fun_name body; cases; default; label }
  | Case (exp, stmt, label) -> Case (exp, resolve_statement fun_name stmt, label)
  | Default (stmt, label) -> Default (resolve_statement fun_name stmt, label)
  | (Return _ | Expression _ | Break _ | Continue _ | Null) as ret -> ret

and resolve_block_item fun_name = function
  | S s -> S (resolve_statement fun_name s)
  | D _ as ret -> ret

and resolve_block fun_name = function
  | Block items -> Block (List.map (resolve_block_item fun_name) items)
;;

let resolve_function_decl = function
  | { name = Identifier iden; params; body; ftp; storage } ->
    let first_pass = Option.map (resolve_block (Some iden)) body in
    let body = Option.map (resolve_block None) first_pass in
    { name = Identifier iden; params; body; ftp; storage }
;;

let resolve_declaration = function
  | FunDecl f -> FunDecl (resolve_function_decl f)
  | VarDecl _ as ret -> ret
  | StructDecl _ -> assert false
;;

let resolve_program = function
  | Program dns -> Program (List.map resolve_declaration dns)
;;
