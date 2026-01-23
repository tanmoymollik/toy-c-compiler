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

type struct_info =
  { tag : string
  ; from_curr_scope : bool
  }

(* Maps an original struct tag to a uniquely created struct tag.
   Also stores if it's from the current scope. *)
type struct_map_type = (string, struct_info) Hashtbl.t

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

let copy_struct_map (struct_map : struct_map_type) =
  let dup = Hashtbl.copy struct_map in
  let f _ { tag; _ } = Some { tag; from_curr_scope = false } in
  Hashtbl.filter_map_inplace f dup;
  dup
;;

let rec resolve_type struct_map = function
  | Structure (Identifier tag) ->
    (match Hashtbl.find_opt struct_map tag with
     | Some { tag; _ } -> Structure (Identifier tag)
     | None -> raise (SemanticError ("Specified an undeclared structure type - " ^ tag)))
  | Pointer tp -> Pointer (resolve_type struct_map tp)
  | CArray (tp, sz) -> CArray (resolve_type struct_map tp, sz)
  | FunType { params; ret } ->
    let ret = resolve_type struct_map ret in
    let params = List.map (resolve_type struct_map) params in
    FunType { params; ret }
  | tp -> tp
;;

let rec resolve_expression iden_map struct_map = function
  | Constant _ as ret -> ret
  | CString _ as ret -> ret
  | Var (Identifier iden, etp) -> Var (Identifier (get_iden iden_map iden), etp)
  | Cast { tgt; exp; etp } ->
    Cast
      { tgt = resolve_type struct_map tgt
      ; exp = resolve_expression iden_map struct_map exp
      ; etp
      }
  | Unary (uop, exp, etp) -> Unary (uop, resolve_expression iden_map struct_map exp, etp)
  | TUnary (tuop, prefix, lval, etp) ->
    TUnary (tuop, prefix, resolve_expression iden_map struct_map lval, etp)
  | Binary { bop; lexp; rexp; etp } ->
    Binary
      { bop
      ; lexp = resolve_expression iden_map struct_map lexp
      ; rexp = resolve_expression iden_map struct_map rexp
      ; etp
      }
  | CompoundAssign { bop; lexp; rexp; btp; etp } ->
    CompoundAssign
      { bop
      ; lexp = resolve_expression iden_map struct_map lexp
      ; rexp = resolve_expression iden_map struct_map rexp
      ; btp
      ; etp
      }
  | Assignment { lval; rval; etp } ->
    Assignment
      { lval = resolve_expression iden_map struct_map lval
      ; rval = resolve_expression iden_map struct_map rval
      ; etp
      }
  | Conditional { cnd; lhs; rhs; etp } ->
    Conditional
      { cnd = resolve_expression iden_map struct_map cnd
      ; lhs = resolve_expression iden_map struct_map lhs
      ; rhs = resolve_expression iden_map struct_map rhs
      ; etp
      }
  | FunctionCall (Identifier name, exps, etp) ->
    let name = Identifier (get_iden iden_map name) in
    FunctionCall (name, List.map (resolve_expression iden_map struct_map) exps, etp)
  | Dereference (exp, etp) -> Dereference (resolve_expression iden_map struct_map exp, etp)
  | AddrOf (exp, etp) -> AddrOf (resolve_expression iden_map struct_map exp, etp)
  | Subscript (e1, e2, etp) ->
    Subscript
      ( resolve_expression iden_map struct_map e1
      , resolve_expression iden_map struct_map e2
      , etp )
  | SizeOf (exp, etp) -> SizeOf (resolve_expression iden_map struct_map exp, etp)
  | SizeOfT (tp, etp) -> SizeOfT (resolve_type struct_map tp, etp)
  | Dot { struct_exp; member; etp } ->
    Dot { struct_exp = resolve_expression iden_map struct_map struct_exp; member; etp }
  | Arrow { struct_ptr; member; etp } ->
    Arrow { struct_ptr = resolve_expression iden_map struct_map struct_ptr; member; etp }
;;

let rec resolve_c_initializer iden_map struct_map = function
  | SingleInit (exp, etp) -> SingleInit (resolve_expression iden_map struct_map exp, etp)
  | CompoundInit (c_ins, etp) ->
    let c_ins = List.map (resolve_c_initializer iden_map struct_map) c_ins in
    CompoundInit (c_ins, etp)
;;

let resolve_block_scope_variable_decl (iden_map : iden_map_type) struct_map = function
  | { name = Identifier name; init; vtp; storage } ->
    let vtp = resolve_type struct_map vtp in
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
      { name = Identifier name; init; vtp; storage })
    else (
      let nw_name = Core.make_unique_iden name in
      add_iden iden_map name nw_name false;
      let name = Identifier nw_name in
      let init = Option.map (resolve_c_initializer iden_map struct_map) init in
      { name; init; vtp; storage })
;;

let resolve_file_scope_variable_decl iden_map struct_map = function
  | { name = Identifier name; init; vtp; storage } ->
    let vtp = resolve_type struct_map vtp in
    add_iden iden_map name name true;
    { name = Identifier name; init; vtp; storage }
;;

let resolve_struct_decl struct_map = function
  | { tag = Identifier tag; members } ->
    let unique_tag =
      match Hashtbl.find_opt struct_map tag with
      | Some { from_curr_scope = false; _ } | None ->
        let nw_tag = Core.make_unique_iden tag in
        Hashtbl.replace struct_map tag { tag = nw_tag; from_curr_scope = true };
        nw_tag
      | Some { tag; _ } -> tag
    in
    let processed_members =
      List.map
        (fun { name; mtp } ->
           let mtp = resolve_type struct_map mtp in
           { name; mtp })
        members
    in
    { tag = Identifier unique_tag; members = processed_members }
;;

let resolve_for_init iden_map struct_map = function
  | InitDecl d -> InitDecl (resolve_block_scope_variable_decl iden_map struct_map d)
  | InitExp e ->
    let e = Option.map (resolve_expression iden_map struct_map) e in
    InitExp e
;;

let rec resolve_statement iden_map struct_map = function
  | Return exp -> Return (Option.map (resolve_expression iden_map struct_map) exp)
  | Expression exp -> Expression (resolve_expression iden_map struct_map exp)
  | If { cnd; thn; els } ->
    If
      { cnd = resolve_expression iden_map struct_map cnd
      ; thn = resolve_statement iden_map struct_map thn
      ; els = Option.map (resolve_statement iden_map struct_map) els
      }
  | Label (iden, stmt) -> Label (iden, resolve_statement iden_map struct_map stmt)
  | Compound block ->
    let iden_map = copy_iden_map iden_map in
    let struct_map = copy_struct_map struct_map in
    Compound (resolve_block iden_map struct_map block)
  | While (exp, stmt, iden) ->
    While
      ( resolve_expression iden_map struct_map exp
      , resolve_statement iden_map struct_map stmt
      , iden )
  | DoWhile (stmt, exp, iden) ->
    DoWhile
      ( resolve_statement iden_map struct_map stmt
      , resolve_expression iden_map struct_map exp
      , iden )
  | For { init; cnd; post; body; label } ->
    let iden_map = copy_iden_map iden_map in
    let struct_map = copy_struct_map struct_map in
    let init = resolve_for_init iden_map struct_map init in
    let cnd = Option.map (resolve_expression iden_map struct_map) cnd in
    let post = Option.map (resolve_expression iden_map struct_map) post in
    let body = resolve_statement iden_map struct_map body in
    For { init; cnd; post; body; label }
  | Switch { cnd; body; cases; default; label } ->
    let cnd = resolve_expression iden_map struct_map cnd in
    let body = resolve_statement iden_map struct_map body in
    Switch { cnd; body; cases; default; label }
  | Case (exp, stmt, label) ->
    Case
      ( resolve_expression iden_map struct_map exp
      , resolve_statement iden_map struct_map stmt
      , label )
  | Default (stmt, label) -> Default (resolve_statement iden_map struct_map stmt, label)
  | (Goto _ | Break _ | Continue _ | Null) as ret -> ret

and resolve_block_item iden_map struct_map = function
  | S s -> S (resolve_statement iden_map struct_map s)
  | D d -> D (resolve_declaration iden_map struct_map true d)

and resolve_block iden_map struct_map = function
  | Block items -> Block (List.map (resolve_block_item iden_map struct_map) items)

and resolve_function_decl iden_map struct_map nested = function
  | { name = Identifier iden; params; body; ftp; storage } ->
    let ftp = resolve_type struct_map ftp in
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
    let inner_iden_map = copy_iden_map iden_map in
    let inner_struct_map = copy_struct_map struct_map in
    let resolve_param iden_map = function
      | Identifier name ->
        if exists_var_iden iden_map name
        then raise (SemanticError ("Duplicate function param - " ^ name));
        let nw_name = Core.make_unique_iden name in
        add_iden iden_map name nw_name false;
        Identifier nw_name
    in
    let params = List.map (resolve_param inner_iden_map) params in
    { name = Identifier iden
    ; params
    ; body = Option.map (resolve_block inner_iden_map inner_struct_map) body
    ; ftp
    ; storage
    }

and resolve_declaration iden_map struct_map nested = function
  | FunDecl f -> FunDecl (resolve_function_decl iden_map struct_map nested f)
  | VarDecl v ->
    if nested
    then VarDecl (resolve_block_scope_variable_decl iden_map struct_map v)
    else VarDecl (resolve_file_scope_variable_decl iden_map struct_map v)
  | StructDecl s -> StructDecl (resolve_struct_decl struct_map s)
;;

let resolve_program = function
  | Program dns ->
    let iden_map : iden_map_type = Hashtbl.create 10 in
    let struct_map : struct_map_type = Hashtbl.create 10 in
    Program (List.map (resolve_declaration iden_map struct_map false) dns)
;;
