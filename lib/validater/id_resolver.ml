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
  | C_ast.Constant _ as ret -> ret
  | C_ast.Var (Common.Identifier iden, etp) ->
    C_ast.Var (Common.Identifier (get_iden iden_map iden), etp)
  | C_ast.Cast { tgt; exp; etp } ->
    C_ast.Cast { tgt; exp = resolve_expression iden_map exp; etp }
  | C_ast.Unary (uop, exp, etp) -> C_ast.Unary (uop, resolve_expression iden_map exp, etp)
  | C_ast.TUnary (tuop, prefix, lval, etp) ->
    C_ast.TUnary (tuop, prefix, resolve_expression iden_map lval, etp)
  | C_ast.Binary { bop; lexp; rexp; etp } ->
    C_ast.Binary
      { bop
      ; lexp = resolve_expression iden_map lexp
      ; rexp = resolve_expression iden_map rexp
      ; etp
      }
  | C_ast.CompoundAssign { bop; lexp; rexp; btp; etp } ->
    C_ast.CompoundAssign
      { bop
      ; lexp = resolve_expression iden_map lexp
      ; rexp = resolve_expression iden_map rexp
      ; btp
      ; etp
      }
  | C_ast.Assignment { lval; rval; etp } ->
    C_ast.Assignment
      { lval = resolve_expression iden_map lval
      ; rval = resolve_expression iden_map rval
      ; etp
      }
  | C_ast.Conditional { cnd; lhs; rhs; etp } ->
    C_ast.Conditional
      { cnd = resolve_expression iden_map cnd
      ; lhs = resolve_expression iden_map lhs
      ; rhs = resolve_expression iden_map rhs
      ; etp
      }
  | C_ast.FunctionCall (Common.Identifier name, exps, etp) ->
    let name = Common.Identifier (get_iden iden_map name) in
    C_ast.FunctionCall (name, List.map (resolve_expression iden_map) exps, etp)
  | C_ast.Dereference (exp, etp) ->
    C_ast.Dereference (resolve_expression iden_map exp, etp)
  | C_ast.AddrOf (exp, etp) -> C_ast.AddrOf (resolve_expression iden_map exp, etp)
;;

let resolve_block_scope_variable_decl iden_map = function
  | C_ast.{ name = Common.Identifier name; init; vtp; storage } as ret ->
    (match Hashtbl.find_opt iden_map name with
     | Some prev_entry ->
       if
         prev_entry.from_curr_scope
         && not (prev_entry.has_linkage && storage = Some C_ast.Extern)
       then raise (SemanticError ("Conflicting local declaration - " ^ name))
     | None -> ());
    if storage = Some C_ast.Extern
    then (
      add_iden iden_map name name true;
      ret)
    else (
      let nw_name = make_unique_name name in
      add_iden iden_map name nw_name false;
      let name = Common.Identifier nw_name in
      let init = Option.map (resolve_expression iden_map) init in
      C_ast.{ name; init; vtp; storage })
;;

let resolve_file_scope_variable_decl iden_map = function
  | C_ast.{ name = Common.Identifier name; init; vtp; storage } ->
    add_iden iden_map name name true;
    C_ast.{ name = Common.Identifier name; init; vtp; storage }
;;

let resolve_for_init iden_map = function
  | C_ast.InitDecl d -> C_ast.InitDecl (resolve_block_scope_variable_decl iden_map d)
  | C_ast.InitExp e ->
    let e = Option.map (resolve_expression iden_map) e in
    C_ast.InitExp e
;;

let rec resolve_statement iden_map = function
  | C_ast.Return exp -> C_ast.Return (resolve_expression iden_map exp)
  | C_ast.Expression exp -> C_ast.Expression (resolve_expression iden_map exp)
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd = resolve_expression iden_map cnd
      ; thn = resolve_statement iden_map thn
      ; els = Option.map (resolve_statement iden_map) els
      }
  | C_ast.Label (iden, stmt) -> C_ast.Label (iden, resolve_statement iden_map stmt)
  | C_ast.Compound block ->
    let iden_map = copy_iden_map iden_map in
    C_ast.Compound (resolve_block iden_map block)
  | C_ast.While (exp, stmt, iden) ->
    C_ast.While (resolve_expression iden_map exp, resolve_statement iden_map stmt, iden)
  | C_ast.DoWhile (stmt, exp, iden) ->
    C_ast.DoWhile (resolve_statement iden_map stmt, resolve_expression iden_map exp, iden)
  | C_ast.For { init; cnd; post; body; label } ->
    let iden_map = copy_iden_map iden_map in
    let init = resolve_for_init iden_map init in
    let cnd = Option.map (resolve_expression iden_map) cnd in
    let post = Option.map (resolve_expression iden_map) post in
    let body = resolve_statement iden_map body in
    C_ast.For { init; cnd; post; body; label }
  | C_ast.Switch { cnd; body; cases; default; label } ->
    let cnd = resolve_expression iden_map cnd in
    let body = resolve_statement iden_map body in
    C_ast.Switch { cnd; body; cases; default; label }
  | C_ast.Case (exp, stmt, label) ->
    C_ast.Case (resolve_expression iden_map exp, resolve_statement iden_map stmt, label)
  | C_ast.Default (stmt, label) -> C_ast.Default (resolve_statement iden_map stmt, label)
  | (C_ast.Goto _ | C_ast.Break _ | C_ast.Continue _ | C_ast.Null) as ret -> ret

and resolve_block_item iden_map = function
  | C_ast.S s -> C_ast.S (resolve_statement iden_map s)
  | C_ast.D d -> C_ast.D (resolve_declaration iden_map true d)

and resolve_block iden_map = function
  | C_ast.Block items -> C_ast.Block (List.map (resolve_block_item iden_map) items)

and resolve_function_decl iden_map nested = function
  | C_ast.{ name = Common.Identifier iden; params; body; ftp; storage } ->
    if nested && Option.is_some body
    then raise (SemanticError ("Nested function definition - " ^ iden));
    if nested && storage = Some C_ast.Static
    then raise (SemanticError ("Nested static function - " ^ iden));
    let info = Hashtbl.find_opt iden_map iden in
    (match info with
     | Some { from_curr_scope = true; has_linkage = false; _ } ->
       raise (SemanticError ("Duplicate identifier - " ^ iden))
     | _ -> ());
    add_iden iden_map iden iden true;
    let inner_map = copy_iden_map iden_map in
    let resolve_param iden_map = function
      | Common.Identifier name ->
        if exists_var_iden iden_map name
        then raise (SemanticError ("Duplicate function param - " ^ name));
        let nw_name = make_unique_name name in
        add_iden iden_map name nw_name false;
        Common.Identifier nw_name
    in
    let params = List.map (resolve_param inner_map) params in
    C_ast.
      { name = Common.Identifier iden
      ; params
      ; body = Option.map (resolve_block inner_map) body
      ; ftp
      ; storage
      }

and resolve_declaration iden_map nested = function
  | C_ast.FunDecl f -> C_ast.FunDecl (resolve_function_decl iden_map nested f)
  | C_ast.VarDecl v ->
    if nested
    then C_ast.VarDecl (resolve_block_scope_variable_decl iden_map v)
    else C_ast.VarDecl (resolve_file_scope_variable_decl iden_map v)
;;

let resolve_program = function
  | C_ast.Program dns ->
    let iden_map : iden_map_type = Hashtbl.create 10 in
    C_ast.Program (List.map (resolve_declaration iden_map false) dns)
;;
