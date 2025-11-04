exception SemanticError of string

let var_map : (string, string) Hashtbl.t = Hashtbl.create 100

let exists_var var =
  match Hashtbl.find_opt var_map var with
  | Some _ -> true
  | None -> false
;;

let add_var key vl = Hashtbl.add var_map key vl

let get_var var =
  match Hashtbl.find_opt var_map var with
  | Some v -> v
  | None -> raise (SemanticError ("Use of undeclared variable - " ^ var))
;;

let make_unique_name iden =
  let c = State.get_var_count () in
  Printf.sprintf "%s.%d" iden c
;;

let rec resolve_exp = function
  | C_ast.Constant _ as ret -> ret
  | C_ast.Var (C_ast.Identifier iden) -> C_ast.Var (C_ast.Identifier (get_var iden))
  | C_ast.Unary (uop, exp) -> C_ast.Unary (uop, resolve_exp exp)
  | C_ast.TUnary (tuop, prefix, lval) ->
    (match lval with
     | C_ast.Var _ -> C_ast.TUnary (tuop, prefix, resolve_exp lval)
     | _ -> raise (SemanticError "Invalid lvalue of suffix/postfix operator."))
  | C_ast.Binary { bop; lexp; rexp } ->
    C_ast.Binary { bop; lexp = resolve_exp lexp; rexp = resolve_exp rexp }
  | C_ast.Assignment { aop; lval; rval } ->
    (match lval with
     | C_ast.Var _ ->
       C_ast.Assignment { aop; lval = resolve_exp lval; rval = resolve_exp rval }
     | _ -> raise (SemanticError "Invalid lvalue of assignment operator."))
;;

let resolve_statement = function
  | C_ast.Return exp -> C_ast.Return (resolve_exp exp)
  | C_ast.Expression exp -> C_ast.Expression (resolve_exp exp)
  | Null -> Null
;;

let resolve_declaration = function
  | C_ast.Declaration { name = C_ast.Identifier iden; init } ->
    if exists_var iden
    then raise (SemanticError ("Duplicate variable declaration - " ^ iden));
    let name = make_unique_name iden in
    add_var iden name;
    let init = Option.map resolve_exp init in
    C_ast.Declaration { name = C_ast.Identifier name; init }
;;

let resolve_block_item = function
  | C_ast.S s -> C_ast.S (resolve_statement s)
  | C_ast.D d -> C_ast.D (resolve_declaration d)
;;

let resolve_function_def = function
  | C_ast.Function { name; body } ->
    C_ast.Function { name; body = List.map resolve_block_item body }
;;

let resolve_program = function
  | C_ast.Program f -> C_ast.Program (resolve_function_def f)
;;
