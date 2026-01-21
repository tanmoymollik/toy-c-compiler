open Common
open C_ast

exception SemanticError = Errors.SemanticError

(* Maps switch labels to case expression lists. *)
let label_map : (string, const list) Hashtbl.t = Hashtbl.create 100

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

let rec evaluate_case_expression = function
  | Constant (c, _) -> c
  | Cast { tgt; exp; _ } ->
    let exp = evaluate_case_expression exp in
    (match tgt with
     | Char | SChar | Int -> ConstInt (TypeConverter.convert_to_int exp)
     | UChar | UInt -> ConstUInt (TypeConverter.convert_to_uint exp)
     | Long -> ConstLong (TypeConverter.convert_to_long exp)
     | ULong -> ConstULong (TypeConverter.convert_to_ulong exp)
     | Double | Void | FunType _ | Pointer _ | CArray _ | Structure _ ->
       raise (SemanticError "Invalid cast target for switch-case"))
  | Unary (uop, exp, _) -> TypeConverter.evaluate_unary uop (evaluate_case_expression exp)
  | Binary { bop; lexp; rexp; _ } ->
    TypeConverter.evaluate_binary
      bop
      (evaluate_case_expression lexp)
      (evaluate_case_expression rexp)
  | Conditional { cnd; lhs; rhs; _ } ->
    let cnd = evaluate_case_expression cnd in
    let lhs = evaluate_case_expression lhs in
    let rhs = evaluate_case_expression rhs in
    TypeConverter.evaluate_conditional cnd lhs rhs
  | CString _
  | Var _
  | TUnary _
  | CompoundAssign _
  | Assignment _
  | FunctionCall _
  | Dereference _
  | AddrOf _
  | Subscript _
  | C_ast.SizeOf _
  | C_ast.SizeOfT _
  | C_ast.Dot _
  | C_ast.Arrow _ -> raise (SemanticError "Non-const value for switch-case")
;;

let rec resolve_statement = function
  | If { cnd; thn; els } ->
    If { cnd; thn = resolve_statement thn; els = Option.map resolve_statement els }
  | Label (lbl, stmt) -> Label (lbl, resolve_statement stmt)
  | Compound block -> Compound (resolve_block block)
  | While (exp, stmt, iden) -> While (exp, resolve_statement stmt, iden)
  | DoWhile (stmt, exp, iden) -> DoWhile (resolve_statement stmt, exp, iden)
  | For { init; cnd; post; body; label } ->
    For { init; cnd; post; body = resolve_statement body; label }
  | Switch { cnd; body; label = Identifier label; _ } ->
    let body = resolve_statement body in
    (* All cases are now added to the label_map. *)
    let cases = List.rev (get_for_label label) in
    let default = exists_default label in
    Switch { cnd; body; cases; default; label = Identifier label }
  | Case (exp, stmt, Identifier lbl) ->
    let v = evaluate_case_expression exp in
    if exists_label lbl v then raise (SemanticError "Case already exists.");
    let id = add_for_label lbl v in
    let lbl = Identifier (Core.case_label id lbl) in
    Label (lbl, resolve_statement stmt)
  | Default (stmt, Identifier lbl) ->
    if exists_default lbl
    then raise (SemanticError "Two default case for a single switch statement");
    add_default lbl;
    let lbl = Identifier (Core.default_label lbl) in
    Label (lbl, resolve_statement stmt)
  | (Return _ | Expression _ | Goto _ | Break _ | Continue _ | Null) as ret -> ret

and resolve_block_item = function
  | S s -> S (resolve_statement s)
  | D _ as ret -> ret

and resolve_block = function
  | Block items -> Block (List.map resolve_block_item items)
;;

let resolve_function_decl = function
  | { name; params; body; ftp; storage } ->
    let body = Option.map resolve_block body in
    { name; params; body; ftp; storage }
;;

let resolve_declaration = function
  | FunDecl f -> FunDecl (resolve_function_decl f)
  | VarDecl _ as ret -> ret
  | StructDecl _ -> assert false
;;

let resolve_program = function
  | Program dns -> Program (List.map resolve_declaration dns)
;;
