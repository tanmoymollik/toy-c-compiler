exception SemanticError = Errors.SemanticError

(* Maps switch labels to case expression lists. *)
let label_map : (string, C_ast.const list) Hashtbl.t = Hashtbl.create 100

(* Maps switch labels to default existence. *)
let default_map : (string, bool) Hashtbl.t = Hashtbl.create 10

let exists_label lbl v =
  match Hashtbl.find_opt label_map lbl with
  | Some l -> List.exists (fun x -> x = v) l
  | _ -> false
;;

let get_for_label lbl =
  match Hashtbl.find_opt label_map lbl with
  | Some v -> v
  | None -> []
;;

let add_for_label k v =
  let l = get_for_label k in
  Hashtbl.replace label_map k (v :: l);
  List.length l
;;

let exists_default var =
  match Hashtbl.find_opt default_map var with
  | Some _ -> true
  | _ -> false
;;

let add_default k = Hashtbl.add default_map k true

let evaluate_unary_expression uop x =
  match uop with
  | C_ast.Complement -> lnot x
  | C_ast.Negate -> -x
  | C_ast.Not -> if x = 0 then 1 else 0
;;

let rec evaluate_case_expression = function
  | C_ast.Constant (c, _) -> c
  | C_ast.Var _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Cast { tgt; exp; _ } ->
    let exp = evaluate_case_expression exp in
    (match tgt with
     | C_ast.Int -> C_ast.ConstInt (Type_converter.convert_to_int exp)
     | C_ast.Long -> C_ast.ConstLong (Type_converter.convert_to_long exp)
     | _ -> assert false)
  | C_ast.Unary (uop, exp, _) ->
    Type_converter.evaluate_unary_expression uop (evaluate_case_expression exp)
  | C_ast.TUnary _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Binary { bop; lexp; rexp; _ } ->
    Type_converter.evaluate_binary_expression
      bop
      (evaluate_case_expression lexp)
      (evaluate_case_expression rexp)
  | C_ast.Assignment _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Conditional { cnd; lhs; rhs; _ } ->
    let cnd = evaluate_case_expression cnd in
    let lhs = evaluate_case_expression lhs in
    let rhs = evaluate_case_expression rhs in
    Type_converter.evaluate_conditional_expression cnd lhs rhs
  | C_ast.FunctionCall _ -> raise (SemanticError "Non-const value for switch-case")
;;

let rec resolve_statement = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If { cnd; thn = resolve_statement thn; els = Option.map resolve_statement els }
  | C_ast.Label (lbl, stmt) -> C_ast.Label (lbl, resolve_statement stmt)
  | C_ast.Compound block -> C_ast.Compound (resolve_block block)
  | C_ast.While (exp, stmt, iden) -> C_ast.While (exp, resolve_statement stmt, iden)
  | C_ast.DoWhile (stmt, exp, iden) -> C_ast.DoWhile (resolve_statement stmt, exp, iden)
  | C_ast.For { init; cnd; post; body; label } ->
    C_ast.For { init; cnd; post; body = resolve_statement body; label }
  | C_ast.Switch { cnd; body; label = C_ast.Identifier label; _ } ->
    let body = resolve_statement body in
    (* All cases are now added to the label_map. *)
    let cases = List.rev (get_for_label label) in
    let default = exists_default label in
    C_ast.Switch { cnd; body; cases; default; label = C_ast.Identifier label }
  | C_ast.Case (exp, stmt, C_ast.Identifier lbl) ->
    let v = evaluate_case_expression exp in
    if exists_label lbl v then raise (SemanticError "Case already exists.");
    let id = add_for_label lbl v in
    let lbl = C_ast.Identifier (Core.case_label id lbl) in
    C_ast.Label (lbl, resolve_statement stmt)
  | C_ast.Default (stmt, C_ast.Identifier lbl) ->
    if exists_default lbl
    then raise (SemanticError "Two default case for a single switch statement");
    add_default lbl;
    let lbl = C_ast.Identifier (Core.default_label lbl) in
    C_ast.Label (lbl, resolve_statement stmt)
  | ( C_ast.Return _
    | C_ast.Expression _
    | C_ast.Goto _
    | C_ast.Break _
    | C_ast.Continue _
    | C_ast.Null ) as ret -> ret

and resolve_block_item = function
  | C_ast.S s -> C_ast.S (resolve_statement s)
  | C_ast.D _ as ret -> ret

and resolve_block = function
  | C_ast.Block items -> C_ast.Block (List.map resolve_block_item items)
;;

let resolve_function_decl = function
  | C_ast.{ name; params; body; ftp; storage } ->
    let body = Option.map resolve_block body in
    C_ast.{ name; params; body; ftp; storage }
;;

let resolve_declaration = function
  | C_ast.FunDecl f -> C_ast.FunDecl (resolve_function_decl f)
  | C_ast.VarDecl _ as ret -> ret
;;

let resolve_program = function
  | C_ast.Program dns -> C_ast.Program (List.map resolve_declaration dns)
;;
