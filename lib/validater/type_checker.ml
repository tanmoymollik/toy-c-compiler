exception SemanticError = Errors.SemanticError

(* Maps switch labels to their condition type. *)
let switch_label_map : (string, C_ast.c_type) Hashtbl.t = Hashtbl.create 100
let add_for_label k v = Hashtbl.replace switch_label_map k v

let get_for_label lbl =
  match Hashtbl.find_opt switch_label_map lbl with
  | Some v -> v
  | None -> assert false
;;

(* size of type in bytes. *)
let size = function
  | C_ast.Int -> 4
  | C_ast.UInt -> 4
  | C_ast.Long -> 8
  | C_ast.ULong -> 8
  | C_ast.FunType _ -> assert false
;;

let signed = function
  | C_ast.Int -> true
  | C_ast.UInt -> false
  | C_ast.Long -> true
  | C_ast.ULong -> false
  | C_ast.FunType _ -> assert false
;;

let get_common_type t1 t2 =
  if t1 = t2
  then t1
  else if size t1 = size t2
  then if signed t1 then t2 else t1
  else if size t1 > size t2
  then t1
  else t2
;;

let convert_to t exp =
  if C_ast.get_type exp = t then exp else C_ast.Cast { tgt = t; exp; etp = t }
;;

let rec typecheck_expression symbol_map = function
  | C_ast.Constant (c, _) -> C_ast.Constant (c, Type_converter.const_type c)
  | C_ast.Var (C_ast.Identifier iden, _) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp = C_ast.FunType _; _ } ->
       raise (SemanticError ("Function name used as variable - " ^ iden))
     | Some Core.{ tp; _ } -> C_ast.Var (C_ast.Identifier iden, tp)
     | None -> assert false)
  | C_ast.Cast { tgt; exp; _ } ->
    C_ast.Cast { tgt; exp = typecheck_expression symbol_map exp; etp = tgt }
  | C_ast.Unary (uop, exp, _) ->
    let exp = typecheck_expression symbol_map exp in
    let etp =
      match uop with
      | C_ast.Not -> C_ast.Int
      | _ -> C_ast.get_type exp
    in
    C_ast.Unary (uop, exp, etp)
  | C_ast.TUnary (uop, p, lval, _) ->
    let lval = typecheck_expression symbol_map lval in
    C_ast.TUnary (uop, p, lval, C_ast.get_type lval)
  | C_ast.Binary { bop; lexp; rexp; _ } ->
    let lexp = typecheck_expression symbol_map lexp in
    let rexp = typecheck_expression symbol_map rexp in
    let convert lexp rexp =
      let t1 = C_ast.get_type lexp in
      let t2 = C_ast.get_type rexp in
      let common_t = get_common_type t1 t2 in
      convert_to common_t lexp, convert_to common_t rexp
    in
    (match bop with
     | C_ast.And | C_ast.Or -> C_ast.Binary { bop; lexp; rexp; etp = C_ast.Int }
     | C_ast.Add
     | C_ast.Sub
     | C_ast.Mul
     | C_ast.Div
     | C_ast.Rem
     | C_ast.BAnd
     | C_ast.BOr
     | C_ast.Xor
     | C_ast.Equal
     | C_ast.NEqual
     | C_ast.LEqual
     | C_ast.GEqual
     | C_ast.Less
     | C_ast.Greater ->
       let lexp, rexp = convert lexp rexp in
       C_ast.Binary { bop; lexp; rexp; etp = C_ast.get_type lexp }
     | C_ast.Lsft | C_ast.Rsft ->
       C_ast.Binary { bop; lexp; rexp; etp = C_ast.get_type lexp })
  | C_ast.Assignment { lval; rval; _ } ->
    let lval = typecheck_expression symbol_map lval in
    let rval = typecheck_expression symbol_map rval in
    let etp = C_ast.get_type lval in
    let rval = convert_to etp rval in
    C_ast.Assignment { lval; rval; etp }
  | C_ast.Conditional { cnd; lhs; rhs; _ } ->
    let lhs = typecheck_expression symbol_map lhs in
    let rhs = typecheck_expression symbol_map rhs in
    let t1 = C_ast.get_type lhs in
    let t2 = C_ast.get_type rhs in
    let common_t = get_common_type t1 t2 in
    let lhs = convert_to common_t lhs in
    let rhs = convert_to common_t rhs in
    C_ast.Conditional
      { cnd = typecheck_expression symbol_map cnd; lhs; rhs; etp = common_t }
  | C_ast.FunctionCall (C_ast.Identifier iden, exps, _) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp = FunType { params; ret }; _ } ->
       if List.length params <> List.length exps
       then
         raise
           (SemanticError ("Function called with wrong number of arguments - " ^ iden));
       let exps = List.map (typecheck_expression symbol_map) exps in
       let converted_args = List.map2 convert_to params exps in
       C_ast.FunctionCall (C_ast.Identifier iden, converted_args, ret)
     | _ -> raise (SemanticError ("Variable used as function - " ^ iden)))
;;

let typecheck_file_scope_variable_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; init; vtp; storage } ->
    let initial_value =
      ref
        (match init with
         | Some (C_ast.Constant (c, _)) ->
           (match vtp with
            | C_ast.Int -> Core.Initial (Core.IntInit (Type_converter.convert_to_int c))
            | C_ast.UInt ->
              Core.Initial (Core.UIntInit (Type_converter.convert_to_uint c))
            | C_ast.Long ->
              Core.Initial (Core.LongInit (Type_converter.convert_to_long c))
            | C_ast.ULong ->
              Core.Initial (Core.ULongInit (Type_converter.convert_to_ulong c))
            | C_ast.FunType _ -> assert false)
         | None -> if storage = Some C_ast.Extern then Core.NoInitial else Core.Tentative
         | _ ->
           raise
             (SemanticError ("Non-constant initializer for file-scope variable - " ^ iden)))
    in
    let global = ref (storage <> Some C_ast.Static) in
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp; attrs } ->
       if tp <> vtp
       then raise (SemanticError ("Variable declaration type mismatch - " ^ iden));
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
    let info = Core.{ tp = vtp; attrs } in
    Hashtbl.replace symbol_map iden info
;;

let typecheck_block_scope_variable_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; init; vtp; storage } as ret ->
    (match storage with
     | Some C_ast.Extern ->
       if init <> None
       then
         raise
           (SemanticError ("Initializer for local extern variable declaration - " ^ iden));
       (match Hashtbl.find_opt symbol_map iden with
        | Some Core.{ tp; _ } ->
          if tp <> vtp
          then raise (SemanticError ("Variable declaration type mismatch - " ^ iden))
        | None ->
          let info =
            Core.{ tp = vtp; attrs = StaticAttr { init = Core.NoInitial; global = true } }
          in
          Hashtbl.replace symbol_map iden info);
       ret
     | Some C_ast.Static ->
       let initial_value =
         match init with
         | Some (C_ast.Constant (c, _)) -> c
         | None -> C_ast.ConstInt Int32.zero
         | _ ->
           raise
             (SemanticError
                ("Non-constant initializer for local static variable - " ^ iden))
       in
       let initial_value =
         match vtp with
         | C_ast.Int ->
           Core.Initial (Core.IntInit (Type_converter.convert_to_int initial_value))
         | C_ast.Long ->
           Core.Initial (Core.LongInit (Type_converter.convert_to_long initial_value))
         | _ -> assert false
       in
       let info =
         Core.{ tp = vtp; attrs = StaticAttr { init = initial_value; global = false } }
       in
       Hashtbl.replace symbol_map iden info;
       ret
     | None ->
       let info = Core.{ tp = vtp; attrs = LocalAttr } in
       Hashtbl.replace symbol_map iden info;
       let init = Option.map (typecheck_expression symbol_map) init in
       let init = Option.map (convert_to vtp) init in
       C_ast.{ name = C_ast.Identifier iden; init; vtp; storage })
;;

let typecheck_for_init symbol_map = function
  | C_ast.InitDecl d ->
    if d.storage <> None then raise (SemanticError "Storage specifier in for-loop init");
    C_ast.InitDecl (typecheck_block_scope_variable_decl symbol_map d)
  | C_ast.InitExp e -> C_ast.InitExp (Option.map (typecheck_expression symbol_map) e)
;;

let rec typecheck_statement symbol_map ftp = function
  | C_ast.Return exp ->
    let exp = typecheck_expression symbol_map exp in
    let exp = convert_to ftp exp in
    C_ast.Return exp
  | C_ast.Expression exp -> C_ast.Expression (typecheck_expression symbol_map exp)
  | C_ast.If { cnd; thn; els } ->
    let cnd = typecheck_expression symbol_map cnd in
    let thn = typecheck_statement symbol_map ftp thn in
    let els = Option.map (typecheck_statement symbol_map ftp) els in
    C_ast.If { cnd; thn; els }
  | C_ast.Label (lbl, stmt) -> C_ast.Label (lbl, typecheck_statement symbol_map ftp stmt)
  | C_ast.Compound block -> C_ast.Compound (typecheck_block symbol_map ftp block)
  | C_ast.While (exp, stmt, lbl) ->
    let exp = typecheck_expression symbol_map exp in
    let stmt = typecheck_statement symbol_map ftp stmt in
    C_ast.While (exp, stmt, lbl)
  | C_ast.DoWhile (stmt, exp, lbl) ->
    let stmt = typecheck_statement symbol_map ftp stmt in
    let exp = typecheck_expression symbol_map exp in
    C_ast.DoWhile (stmt, exp, lbl)
  | C_ast.For { init; cnd; post; body; label } ->
    let init = typecheck_for_init symbol_map init in
    let cnd = Option.map (typecheck_expression symbol_map) cnd in
    let post = Option.map (typecheck_expression symbol_map) post in
    let body = typecheck_statement symbol_map ftp body in
    C_ast.For { init; cnd; post; body; label }
  | C_ast.Switch { cnd; body; cases; default; label = C_ast.Identifier label } ->
    let cnd = typecheck_expression symbol_map cnd in
    add_for_label label (C_ast.get_type cnd);
    let body = typecheck_statement symbol_map ftp body in
    C_ast.Switch { cnd; body; cases; default; label = C_ast.Identifier label }
  | C_ast.Case (exp, stmt, C_ast.Identifier lbl) ->
    let exp = typecheck_expression symbol_map exp in
    let etp = get_for_label lbl in
    let exp = convert_to etp exp in
    let stmt = typecheck_statement symbol_map ftp stmt in
    C_ast.Case (exp, stmt, C_ast.Identifier lbl)
  | C_ast.Default (stmt, lbl) ->
    let stmt = typecheck_statement symbol_map ftp stmt in
    C_ast.Default (stmt, lbl)
  | (C_ast.Goto _ | C_ast.Break _ | C_ast.Continue _ | C_ast.Null) as ret -> ret

and typecheck_block_item symbol_map ftp = function
  | C_ast.S s -> C_ast.S (typecheck_statement symbol_map ftp s)
  | C_ast.D d -> C_ast.D (typecheck_declaration symbol_map true d)

and typecheck_block symbol_map ftp = function
  | C_ast.Block items ->
    let items = List.map (typecheck_block_item symbol_map ftp) items in
    C_ast.Block items

and typecheck_function_decl symbol_map = function
  | C_ast.{ name = C_ast.Identifier iden; params; body; ftp; storage } ->
    let has_body = Option.is_some body in
    let already_defined = ref false in
    let global = ref (storage <> Some C_ast.Static) in
    (match Hashtbl.find_opt symbol_map iden with
     | Some Core.{ tp; attrs } ->
       if ftp <> tp
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
    let info = Core.{ tp = ftp; attrs } in
    Hashtbl.replace symbol_map iden info;
    let typecheck_param symbol_map ptp = function
      | C_ast.Identifier name ->
        Hashtbl.replace symbol_map name Core.{ tp = ptp; attrs = LocalAttr }
    in
    let ptps, rtp =
      match ftp with
      | C_ast.FunType { params; ret } -> params, ret
      | _ -> assert false
    in
    let _ = List.map2 (typecheck_param symbol_map) ptps params in
    let body = Option.map (typecheck_block symbol_map rtp) body in
    C_ast.{ name = C_ast.Identifier iden; params; body; ftp; storage }

and typecheck_declaration symbol_map nested = function
  | C_ast.FunDecl f -> C_ast.FunDecl (typecheck_function_decl symbol_map f)
  | C_ast.VarDecl v ->
    if nested
    then C_ast.VarDecl (typecheck_block_scope_variable_decl symbol_map v)
    else (
      typecheck_file_scope_variable_decl symbol_map v;
      C_ast.VarDecl v)
;;

let typecheck_program = function
  | C_ast.Program dns ->
    C_ast.Program (List.map (typecheck_declaration Core.symbol_map false) dns)
;;
