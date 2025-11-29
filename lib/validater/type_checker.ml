exception SemanticError = Common.SemanticError

let tmp_inner_const = function
  | C_ast.ConstInt i -> i
  | C_ast.ConstLong l -> Int64.to_int l
;;

let rec typecheck_expression symbol_map = function
  | C_ast.Constant _ -> ()
  | C_ast.Var (C_ast.Identifier iden) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp = Core.Int; _ } -> ()
     | _ -> raise (SemanticError ("Function name used as variable - " ^ iden)))
  | C_ast.Cast { exp; _ } -> typecheck_expression symbol_map exp
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

let typecheck_file_scope_variable_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; init; storage } ->
    (* init is a constant. No need to typecheck expression later. *)
    let initial_value =
      ref
        (match init with
         | Some (C_ast.Constant c) -> Core.Initial (tmp_inner_const c)
         | None -> if storage = Some C_ast.Extern then Core.NoInitial else Core.Tentative
         | _ ->
           raise
             (SemanticError ("Non-constant initializer for file-scope variable - " ^ iden)))
    in
    let global = ref (storage <> Some C_ast.Static) in
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp; attrs } ->
       if tp <> Core.Int
       then raise (SemanticError ("Function declared as variable - " ^ iden));
       (match attrs with
        | StaticAttr attrs ->
          if storage = Some C_ast.Extern
          then global := attrs.global
          else if attrs.global <> !global
          then raise (SemanticError ("Conflicting variable linkage - " ^ iden));
          let is_constant init =
            match init with
            | Core.Initial _ -> true
            | _ -> false
          in
          if is_constant attrs.init
          then
            if is_constant !initial_value
            then raise (SemanticError ("Redefinition of constant variable - " ^ iden))
            else initial_value := attrs.init
          else if (not (is_constant !initial_value)) && attrs.init = Core.Tentative
          then initial_value := Core.Tentative
        | _ -> assert false)
     | _ -> ());
    let attrs = Core.StaticAttr { init = !initial_value; global = !global } in
    let info = Core.{ tp = Core.Int; attrs } in
    Hashtbl.replace symbol_map iden info
;;

let typecheck_block_scope_variable_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; init; storage } ->
    (match storage with
     | Some C_ast.Extern ->
       if init <> None
       then
         raise
           (SemanticError ("Initializer for local extern variable declaration - " ^ iden));
       (match Hashtbl.find_opt symbol_map iden with
        | Some Core.{ tp; _ } ->
          if tp <> Core.Int
          then raise (SemanticError ("Function declared as variable - " ^ iden))
        | None ->
          let info =
            Core.{ tp = Core.Int; attrs = StaticAttr { init = NoInitial; global = true } }
          in
          Hashtbl.replace symbol_map iden info)
     | Some C_ast.Static ->
       let initial_value =
         match init with
         | Some (C_ast.Constant c) -> Core.Initial (tmp_inner_const c)
         | None -> Core.Initial 0
         | _ ->
           raise
             (SemanticError
                ("Non-constant initializer for local static variable - " ^ iden))
       in
       let info =
         Core.
           { tp = Core.Int; attrs = StaticAttr { init = initial_value; global = false } }
       in
       Hashtbl.replace symbol_map iden info
     | None ->
       let info = Core.{ tp = Core.Int; attrs = LocalAttr } in
       Hashtbl.replace symbol_map iden info;
       let _ = Option.map (typecheck_expression symbol_map) init in
       ())
;;

let typecheck_for_init symbol_map = function
  | C_ast.InitDecl d ->
    if d.storage <> None then raise (SemanticError "Storage specifier in for-loop init");
    typecheck_block_scope_variable_decl symbol_map d
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
  | C_ast.D d -> typecheck_declaration symbol_map true d

and typecheck_block symbol_map = function
  | C_ast.Block items ->
    let _ = List.map (typecheck_block_item symbol_map) items in
    ()

and typecheck_function_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; params; body; storage } ->
    let fun_type = Core.FunType (List.length params) in
    let has_body = Option.is_some body in
    let already_defined = ref false in
    let global = ref (storage <> Some C_ast.Static) in
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp; attrs } ->
       if fun_type <> tp
       then raise (SemanticError ("Incomplete function declaration - " ^ iden));
       (match attrs with
        | Core.FunAttr attrs ->
          already_defined := attrs.defined;
          if !already_defined && has_body
          then raise (SemanticError ("Function is defined more thand once - " ^ iden));
          if attrs.global && storage = Some C_ast.Static
          then
            raise
              (SemanticError ("Static function declaration follows non-static - " ^ iden));
          global := attrs.global
        | _ -> assert false)
     | _ -> ());
    let attrs =
      Core.FunAttr { defined = !already_defined || has_body; global = !global }
    in
    let info = Core.{ tp = fun_type; attrs } in
    Hashtbl.replace symbol_map iden info;
    let typecheck_param symbol_map = function
      | C_ast.Identifier name ->
        Hashtbl.replace symbol_map name Core.{ tp = Int; attrs = LocalAttr }
    in
    let _ = List.map (typecheck_param symbol_map) params in
    let f body = typecheck_block symbol_map body in
    let _ = Option.map f body in
    ()

and typecheck_declaration symbol_map nested = function
  | C_ast.FunDecl f -> typecheck_function_decl symbol_map f
  | C_ast.VarDecl v ->
    if nested
    then typecheck_block_scope_variable_decl symbol_map v
    else typecheck_file_scope_variable_decl symbol_map v
;;

let typecheck_program = function
  | C_ast.Program dns as ret ->
    let _ = List.map (typecheck_declaration Core.symbol_map false) dns in
    ret
;;
