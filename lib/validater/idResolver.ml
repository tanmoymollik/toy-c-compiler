open Common
open C_ast

exception SemanticError = Errors.SemanticError

type iden_info =
  { name : string
  ; from_curr_scope : bool
  ; has_linkage : bool
  }

(* Maps an original identifier to a uniquely created identifier.
   Also stores if it's from the current scope and if it has external linkage. *)
type iden_map_type = (string, iden_info) Hashtbl.t

let exists_var_iden (iden_map : iden_map_type) iden =
  match Hashtbl.find_opt iden_map iden with
  | Some { from_curr_scope = true; _ } -> true
  | _ -> false
;;

let add_iden (iden_map : iden_map_type) k v linkage =
  Hashtbl.replace iden_map k { name = v; from_curr_scope = true; has_linkage = linkage }
;;

let get_iden (iden_map : iden_map_type) iden =
  match Hashtbl.find_opt iden_map iden with
  | Some { name; _ } -> name
  | None -> raise (SemanticError ("Use of undeclared identifier - " ^ iden))
;;

let copy_iden_map (iden_map : iden_map_type) =
  let dup = Hashtbl.copy iden_map in
  let f _ { name; has_linkage; _ } =
    Some { name; from_curr_scope = false; has_linkage }
  in
  Hashtbl.filter_map_inplace f dup;
  dup
;;

let make_unique_name iden =
  let c = Core.get_var_count () in
  Printf.sprintf "%s.%d" iden c
;;

let rec resolve_expression iden_map = function
  | Constant _ as ret -> ret
  | CString _ as ret -> ret
  | Var (Identifier iden, etp) -> Var (Identifier (get_iden iden_map iden), etp)
  | Cast { tgt; exp; etp } -> Cast { tgt; exp = resolve_expression iden_map exp; etp }
  | Unary (uop, exp, etp) -> Unary (uop, resolve_expression iden_map exp, etp)
  | TUnary (tuop, prefix, lval, etp) ->
    TUnary (tuop, prefix, resolve_expression iden_map lval, etp)
  | Binary { bop; lexp; rexp; etp } ->
    Binary
      { bop
      ; lexp = resolve_expression iden_map lexp
      ; rexp = resolve_expression iden_map rexp
      ; etp
      }
  | CompoundAssign { bop; lexp; rexp; btp; etp } ->
    CompoundAssign
      { bop
      ; lexp = resolve_expression iden_map lexp
      ; rexp = resolve_expression iden_map rexp
      ; btp
      ; etp
      }
  | Assignment { lval; rval; etp } ->
    Assignment
      { lval = resolve_expression iden_map lval
      ; rval = resolve_expression iden_map rval
      ; etp
      }
  | Conditional { cnd; lhs; rhs; etp } ->
    Conditional
      { cnd = resolve_expression iden_map cnd
      ; lhs = resolve_expression iden_map lhs
      ; rhs = resolve_expression iden_map rhs
      ; etp
      }
  | FunctionCall (Identifier name, exps, etp) ->
    let name = Identifier (get_iden iden_map name) in
    FunctionCall (name, List.map (resolve_expression iden_map) exps, etp)
  | Dereference (exp, etp) -> Dereference (resolve_expression iden_map exp, etp)
  | AddrOf (exp, etp) -> AddrOf (resolve_expression iden_map exp, etp)
  | Subscript (e1, e2, etp) ->
    Subscript (resolve_expression iden_map e1, resolve_expression iden_map e2, etp)
  | SizeOf (exp, etp) -> SizeOf (resolve_expression iden_map exp, etp)
  | SizeOfT _ as x -> x
;;

let rec resolve_c_initializer iden_map = function
  | SingleInit (exp, etp) -> SingleInit (resolve_expression iden_map exp, etp)
  | CompoundInit (c_ins, etp) ->
    let c_ins = List.map (resolve_c_initializer iden_map) c_ins in
    CompoundInit (c_ins, etp)
;;

let resolve_block_scope_variable_decl iden_map = function
  | { name = Identifier name; init; vtp; storage } as ret ->
    (match Hashtbl.find_opt iden_map name with
     | Some prev_entry ->
       if
         prev_entry.from_curr_scope
         && not (prev_entry.has_linkage && storage = Some Extern)
       then raise (SemanticError ("Conflicting local declaration - " ^ name))
     | None -> ());
    if storage = Some Extern
    then (
      add_iden iden_map name name true;
      ret)
    else (
      let nw_name = make_unique_name name in
      add_iden iden_map name nw_name false;
      let name = Identifier nw_name in
      let init = Option.map (resolve_c_initializer iden_map) init in
      { name; init; vtp; storage })
;;

let resolve_file_scope_variable_decl iden_map = function
  | { name = Identifier name; init; vtp; storage } ->
    add_iden iden_map name name true;
    { name = Identifier name; init; vtp; storage }
;;

let resolve_for_init iden_map = function
  | InitDecl d -> InitDecl (resolve_block_scope_variable_decl iden_map d)
  | InitExp e ->
    let e = Option.map (resolve_expression iden_map) e in
    InitExp e
;;

let rec resolve_statement iden_map = function
  | Return exp -> Return (Option.map (resolve_expression iden_map) exp)
  | Expression exp -> Expression (resolve_expression iden_map exp)
  | If { cnd; thn; els } ->
    If
      { cnd = resolve_expression iden_map cnd
      ; thn = resolve_statement iden_map thn
      ; els = Option.map (resolve_statement iden_map) els
      }
  | Label (iden, stmt) -> Label (iden, resolve_statement iden_map stmt)
  | Compound block ->
    let iden_map = copy_iden_map iden_map in
    Compound (resolve_block iden_map block)
  | While (exp, stmt, iden) ->
    While (resolve_expression iden_map exp, resolve_statement iden_map stmt, iden)
  | DoWhile (stmt, exp, iden) ->
    DoWhile (resolve_statement iden_map stmt, resolve_expression iden_map exp, iden)
  | For { init; cnd; post; body; label } ->
    let iden_map = copy_iden_map iden_map in
    let init = resolve_for_init iden_map init in
    let cnd = Option.map (resolve_expression iden_map) cnd in
    let post = Option.map (resolve_expression iden_map) post in
    let body = resolve_statement iden_map body in
    For { init; cnd; post; body; label }
  | Switch { cnd; body; cases; default; label } ->
    let cnd = resolve_expression iden_map cnd in
    let body = resolve_statement iden_map body in
    Switch { cnd; body; cases; default; label }
  | Case (exp, stmt, label) ->
    Case (resolve_expression iden_map exp, resolve_statement iden_map stmt, label)
  | Default (stmt, label) -> Default (resolve_statement iden_map stmt, label)
  | (Goto _ | Break _ | Continue _ | Null) as ret -> ret

and resolve_block_item iden_map = function
  | S s -> S (resolve_statement iden_map s)
  | D d -> D (resolve_declaration iden_map true d)

and resolve_block iden_map = function
  | Block items -> Block (List.map (resolve_block_item iden_map) items)

and resolve_function_decl iden_map nested = function
  | { name = Identifier iden; params; body; ftp; storage } ->
    if nested && Option.is_some body
    then raise (SemanticError ("Nested function definition - " ^ iden));
    if nested && storage = Some Static
    then raise (SemanticError ("Nested static function - " ^ iden));
    let info = Hashtbl.find_opt iden_map iden in
    (match info with
     | Some { from_curr_scope = true; has_linkage = false; _ } ->
       raise (SemanticError ("Duplicate identifier - " ^ iden))
     | _ -> ());
    add_iden iden_map iden iden true;
    let inner_map = copy_iden_map iden_map in
    let resolve_param iden_map = function
      | Identifier name ->
        if exists_var_iden iden_map name
        then raise (SemanticError ("Duplicate function param - " ^ name));
        let nw_name = make_unique_name name in
        add_iden iden_map name nw_name false;
        Identifier nw_name
    in
    let params = List.map (resolve_param inner_map) params in
    { name = Identifier iden
    ; params
    ; body = Option.map (resolve_block inner_map) body
    ; ftp
    ; storage
    }

and resolve_declaration iden_map nested = function
  | FunDecl f -> FunDecl (resolve_function_decl iden_map nested f)
  | VarDecl v ->
    if nested
    then VarDecl (resolve_block_scope_variable_decl iden_map v)
    else VarDecl (resolve_file_scope_variable_decl iden_map v)
;;

let resolve_program = function
  | Program dns ->
    let iden_map : iden_map_type = Hashtbl.create 10 in
    Program (List.map (resolve_declaration iden_map false) dns)
;;
