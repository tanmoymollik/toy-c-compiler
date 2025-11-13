exception SemanticError = Common.SemanticError

let rec typecheck_expression symbol_map = function
  | C_ast.Constant _ -> ()
  | C_ast.Var (C_ast.Identifier iden) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp = Core.Int; _ } -> ()
     | _ -> raise (SemanticError ("Function name used as variable - " ^ iden)))
  | C_ast.Unary (_, exp) -> typecheck_expression symbol_map exp
  | C_ast.TUnary (_, _, lval) -> typecheck_expression symbol_map lval
  | C_ast.Binary { lexp; rexp; _ } ->
    typecheck_expression symbol_map lexp;
    typecheck_expression symbol_map rexp
  | C_ast.Assignment { lval; rval; _ } ->
    typecheck_expression symbol_map lval;
    typecheck_expression symbol_map rval
  | C_ast.Conditional { cnd; lhs; rhs } ->
    typecheck_expression symbol_map cnd;
    typecheck_expression symbol_map lhs;
    typecheck_expression symbol_map rhs
  | C_ast.FunctionCall (C_ast.Identifier iden, exps) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { tp = FunType c; _ } ->
       if c <> List.length exps
       then
         raise
           (SemanticError ("Function called with wrong number of arguments - " ^ iden))
     | _ -> raise (SemanticError ("Variable used as function - " ^ iden)));
    let _ = List.map (typecheck_expression symbol_map) exps in
    ()
;;

let typecheck_variable symbol_map = function
  | C_ast.Identifier name -> Hashtbl.add symbol_map name Core.{ tp = Int; defined = true }
;;

let typecheck_variable_decl symbol_map = function
  | C_ast.{ name; init } ->
    typecheck_variable symbol_map name;
    let _ = Option.map (typecheck_expression symbol_map) init in
    ()
;;

let typecheck_for_init symbol_map = function
  | C_ast.InitDecl d -> typecheck_variable_decl symbol_map d
  | C_ast.InitExp e ->
    let _ = Option.map (typecheck_expression symbol_map) e in
    ()
;;

let rec typecheck_statement symbol_map = function
  | C_ast.Return exp -> typecheck_expression symbol_map exp
  | C_ast.Expression exp -> typecheck_expression symbol_map exp
  | C_ast.If { cnd; thn; els } ->
    typecheck_expression symbol_map cnd;
    typecheck_statement symbol_map thn;
    let _ = Option.map (typecheck_statement symbol_map) els in
    ()
  | C_ast.Label (_, stmt) -> typecheck_statement symbol_map stmt
  | C_ast.Compound block -> typecheck_block symbol_map block
  | C_ast.While (exp, stmt, _) ->
    typecheck_expression symbol_map exp;
    typecheck_statement symbol_map stmt
  | C_ast.DoWhile (stmt, exp, _) ->
    typecheck_statement symbol_map stmt;
    typecheck_expression symbol_map exp
  | C_ast.For { init; cnd; post; body; _ } ->
    typecheck_for_init symbol_map init;
    let _ = Option.map (typecheck_expression symbol_map) cnd in
    let _ = Option.map (typecheck_expression symbol_map) post in
    typecheck_statement symbol_map body
  | C_ast.Switch { cnd; body; _ } ->
    typecheck_expression symbol_map cnd;
    typecheck_statement symbol_map body
  | C_ast.Case (exp, stmt, _) ->
    typecheck_expression symbol_map exp;
    typecheck_statement symbol_map stmt
  | C_ast.Default (stmt, _) -> typecheck_statement symbol_map stmt
  | C_ast.Goto _ | C_ast.Break _ | C_ast.Continue _ | C_ast.Null -> ()

and typecheck_block_item symbol_map = function
  | C_ast.S s -> typecheck_statement symbol_map s
  | C_ast.D d -> typecheck_declaration symbol_map d

and typecheck_block symbol_map = function
  | C_ast.Block items ->
    let _ = List.map (typecheck_block_item symbol_map) items in
    ()

and typecheck_function_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; params; body } ->
    let fun_type = Core.FunType (List.length params) in
    let has_body = Option.is_some body in
    let already_defined = ref false in
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp; defined } ->
       if fun_type <> tp
       then raise (SemanticError ("Incomplete function declaration - " ^ iden));
       already_defined := defined;
       if defined && has_body
       then raise (SemanticError ("Function is defined more thand once - " ^ iden))
     | _ -> ());
    let info = Core.{ tp = fun_type; defined = !already_defined || has_body } in
    Hashtbl.add symbol_map iden info;
    (match body with
     | Some body ->
       let _ = List.map (typecheck_variable symbol_map) params in
       typecheck_block symbol_map body
     | None -> ());
    ()

and typecheck_declaration symbol_map = function
  | C_ast.FunDecl f -> typecheck_function_decl symbol_map f
  | C_ast.VarDecl v -> typecheck_variable_decl symbol_map v
;;

let typecheck_program = function
  | C_ast.Program fns as ret ->
    let _ = List.map (typecheck_function_decl Core.symbol_map) fns in
    ret
;;
