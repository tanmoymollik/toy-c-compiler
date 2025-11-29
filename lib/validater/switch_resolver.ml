exception SemanticError = Common.SemanticError

type label_map_type = (string, int list) Hashtbl.t
type default_map_type = (string, bool) Hashtbl.t

let exists_label (label_map : label_map_type) lbl v =
  match Hashtbl.find_opt label_map lbl with
  | Some l -> List.exists (fun x -> x = v) l
  | _ -> false
;;

let get_for_label (label_map : label_map_type) lbl =
  match Hashtbl.find_opt label_map lbl with
  | Some v -> v
  | None -> []
;;

let add_for_label (label_map : label_map_type) k v =
  let l = get_for_label label_map k in
  Hashtbl.replace label_map k (v :: l);
  List.length l
;;

let exists_default (default_map : default_map_type) var =
  match Hashtbl.find_opt default_map var with
  | Some _ -> true
  | _ -> false
;;

let add_default (default_map : default_map_type) k = Hashtbl.add default_map k true
let make_label prefix = C_ast.Identifier prefix

let evaluate_unary_expression uop x =
  match uop with
  | C_ast.Complement -> lnot x
  | C_ast.Negate -> -x
  | C_ast.Not -> if x = 0 then 1 else 0
;;

let evaluate_binary_expression bop l r =
  match bop with
  | C_ast.Add -> l + r
  | C_ast.Sub -> l - r
  | C_ast.Mul -> l * r
  | C_ast.Div -> l / r
  | C_ast.Rem -> l mod r
  | C_ast.BAnd -> l land r
  | C_ast.BOr -> l lor r
  | C_ast.Xor -> l lxor r
  | C_ast.Lsft -> l lsl r
  | C_ast.Rsft -> l asr r
  | C_ast.And -> if l != 0 && r != 0 then 1 else 0
  | C_ast.Or -> if l != 0 || r != 0 then 1 else 0
  | C_ast.Equal -> if l = r then 1 else 0
  | C_ast.NEqual -> if l <> r then 1 else 0
  | C_ast.LEqual -> if l <= r then 1 else 0
  | C_ast.GEqual -> if l >= r then 1 else 0
  | C_ast.Less -> if l < r then 1 else 0
  | C_ast.Greater -> if l > r then 1 else 0
;;

let rec evaluate_case_expression = function
  | C_ast.Constant c ->
    (match c with
     | C_ast.ConstInt i -> i
     | C_ast.ConstLong l -> Int64.to_int l)
  | C_ast.Var _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Cast _ -> raise (SemanticError "Can't cast in switch-case")
  | C_ast.Unary (uop, exp) -> evaluate_unary_expression uop (evaluate_case_expression exp)
  | C_ast.TUnary _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Binary { bop; lexp; rexp } ->
    evaluate_binary_expression
      bop
      (evaluate_case_expression lexp)
      (evaluate_case_expression rexp)
  | C_ast.Assignment _ -> raise (SemanticError "Non-const value for switch-case")
  | C_ast.Conditional { cnd; lhs; rhs } ->
    let cnd = evaluate_case_expression cnd in
    let lhs = evaluate_case_expression lhs in
    let rhs = evaluate_case_expression rhs in
    if cnd <> 0 then lhs else rhs
  | C_ast.FunctionCall _ -> raise (SemanticError "Non-const value for switch-case")
;;

let rec resolve_statement label_map default_map = function
  | C_ast.If { cnd; thn; els } ->
    C_ast.If
      { cnd
      ; thn = resolve_statement label_map default_map thn
      ; els = Option.map (resolve_statement label_map default_map) els
      }
  | C_ast.Label (lbl, stmt) ->
    C_ast.Label (lbl, resolve_statement label_map default_map stmt)
  | C_ast.Compound block -> C_ast.Compound (resolve_block label_map default_map block)
  | C_ast.While (exp, stmt, iden) ->
    C_ast.While (exp, resolve_statement label_map default_map stmt, iden)
  | C_ast.DoWhile (stmt, exp, iden) ->
    C_ast.DoWhile (resolve_statement label_map default_map stmt, exp, iden)
  | C_ast.For { init; cnd; post; body; label } ->
    C_ast.For
      { init; cnd; post; body = resolve_statement label_map default_map body; label }
  | C_ast.Switch { cnd; body; label = C_ast.Identifier label; _ } ->
    let body = resolve_statement label_map default_map body in
    (* All cases are now added to the label_map. *)
    let cases = List.rev (get_for_label label_map label) in
    let default = exists_default default_map label in
    C_ast.Switch { cnd; body; cases; default; label = C_ast.Identifier label }
  | C_ast.Case (exp, stmt, C_ast.Identifier lbl) ->
    let v = evaluate_case_expression exp in
    if exists_label label_map lbl v
    then raise (SemanticError ("Case " ^ string_of_int v ^ " already exists."));
    let id = add_for_label label_map lbl v in
    let lbl = make_label (Core.case_label id lbl) in
    C_ast.Label (lbl, resolve_statement label_map default_map stmt)
  | C_ast.Default (stmt, C_ast.Identifier lbl) ->
    if exists_default default_map lbl
    then raise (SemanticError "Two default case for a single switch statement");
    add_default default_map lbl;
    let lbl = make_label (Core.default_label lbl) in
    C_ast.Label (lbl, resolve_statement label_map default_map stmt)
  | ( C_ast.Return _
    | C_ast.Expression _
    | C_ast.Goto _
    | C_ast.Break _
    | C_ast.Continue _
    | C_ast.Null ) as ret -> ret

and resolve_block_item label_map default_map = function
  | C_ast.S s -> C_ast.S (resolve_statement label_map default_map s)
  | C_ast.D _ as ret -> ret

and resolve_block label_map default_map = function
  | C_ast.Block items ->
    C_ast.Block (List.map (resolve_block_item label_map default_map) items)
;;

let resolve_function_decl = function
  | C_ast.{ name; params; body; storage } ->
    let label_map : label_map_type = Hashtbl.create 10 in
    let default_map : default_map_type = Hashtbl.create 2 in
    let body = Option.map (resolve_block label_map default_map) body in
    C_ast.{ name; params; body; storage }
;;

let resolve_declaration = function
  | C_ast.FunDecl f -> C_ast.FunDecl (resolve_function_decl f)
  | C_ast.VarDecl _ as ret -> ret
;;

let resolve_program = function
  | C_ast.Program dns -> C_ast.Program (List.map resolve_declaration dns)
;;
