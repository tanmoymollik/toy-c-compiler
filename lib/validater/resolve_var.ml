exception SemanticError = Common.SemanticError

type var_map_type = (string, string * bool) Hashtbl.t

let exists_var (var_map : var_map_type) var =
  match Hashtbl.find_opt var_map var with
  | Some (_, true) -> true
  | _ -> false
;;

let add_var (var_map : var_map_type) k v = Hashtbl.add var_map k (v, true)

let get_var (var_map : var_map_type) var =
  match Hashtbl.find_opt var_map var with
  | Some (v, _) -> v
  | None -> raise (SemanticError ("Use of undeclared variable - " ^ var))
;;

let copy_var_map (var_map : var_map_type) =
  let dup = Hashtbl.copy var_map in
  let f _ (v, _) = Some (v, false) in
  Hashtbl.filter_map_inplace f dup;
  dup
;;

let make_unique_name iden =
  let c = State.get_var_count () in
  Printf.sprintf "%s.%d" iden c
;;

let rec resolve_exp var_map = function
  | C_ast.Constant _ as ret -> ret
  | C_ast.Var (C_ast.Identifier iden) ->
    C_ast.Var (C_ast.Identifier (get_var var_map iden))
  | C_ast.Unary (uop, exp) -> C_ast.Unary (uop, resolve_exp var_map exp)
  | C_ast.TUnary (tuop, prefix, lval) ->
    (match lval with
     | C_ast.Var _ -> C_ast.TUnary (tuop, prefix, resolve_exp var_map lval)
     | _ -> raise (SemanticError "Invalid lvalue of suffix/postfix operator."))
  | C_ast.Binary { bop; lexp; rexp } ->
    C_ast.Binary { bop; lexp = resolve_exp var_map lexp; rexp = resolve_exp var_map rexp }
  | C_ast.Assignment { aop; lval; rval } ->
    (match lval with
     | C_ast.Var _ ->
       C_ast.Assignment
         { aop; lval = resolve_exp var_map lval; rval = resolve_exp var_map rval }
     | _ -> raise (SemanticError "Invalid lvalue of assignment operator."))
  | C_ast.Conditional { cnd; lhs; rhs } ->
    C_ast.Conditional
      { cnd = resolve_exp var_map cnd
      ; lhs = resolve_exp var_map lhs
      ; rhs = resolve_exp var_map rhs
      }
;;

let resolve_declaration var_map = function
  | C_ast.Declaration { name = C_ast.Identifier iden; init } ->
    if exists_var var_map iden
    then raise (SemanticError ("Duplicate variable declaration - " ^ iden));
    let name = make_unique_name iden in
    add_var var_map iden name;
    let init = Option.map (resolve_exp var_map) init in
    C_ast.Declaration { name = C_ast.Identifier name; init }
;;

let rec resolve_statement var_map = function
  | C_ast.Return exp -> C_ast.Return (resolve_exp var_map exp)
  | C_ast.Expression exp -> C_ast.Expression (resolve_exp var_map exp)
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd = resolve_exp var_map cnd
      ; thn = resolve_statement var_map thn
      ; els = Option.map (resolve_statement var_map) els
      }
  | C_ast.Label ((C_ast.Identifier label as lbl), stmt) ->
    if State.exists_label label
    then raise (SemanticError ("duplicate label: " ^ label))
    else (
      State.add_label label;
      C_ast.Label (lbl, resolve_statement var_map stmt))
  | C_ast.Compound block -> C_ast.Compound (resolve_block var_map block)
  | _ as ret -> ret

and resolve_block_item var_map = function
  | C_ast.S s -> C_ast.S (resolve_statement var_map s)
  | C_ast.D d -> C_ast.D (resolve_declaration var_map d)

and resolve_block var_map = function
  | C_ast.Block items ->
    let var_map = copy_var_map var_map in
    C_ast.Block (List.map (resolve_block_item var_map) items)
;;

let resolve_function_def var_map = function
  | C_ast.Function { name; body } ->
    C_ast.Function { name; body = resolve_block var_map body }
;;

let resolve_program = function
  | C_ast.Program f ->
    let var_map : var_map_type = Hashtbl.create 10 in
    C_ast.Program (resolve_function_def var_map f)
;;
