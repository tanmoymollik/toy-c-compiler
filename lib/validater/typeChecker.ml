open Common
open C_ast

exception SemanticError = Errors.SemanticError

(* Maps switch labels to their condition type. *)
let switch_label_map : (string, c_type) Hashtbl.t = Hashtbl.create 100
let add_for_label k v = Hashtbl.replace switch_label_map k v

let get_for_label lbl =
  match Hashtbl.find_opt switch_label_map lbl with
  | Some v -> v
  | None -> assert false
;;

let rec validate_type_specifier = function
  | CArray (tp, _) ->
    if is_complete tp |> not then raise (SemanticError "Illegal array of incomplete type");
    validate_type_specifier tp
  | Pointer tp -> validate_type_specifier tp
  | FunType { params; ret } ->
    List.iter validate_type_specifier params;
    validate_type_specifier ret
  | _ -> ()
;;

let typecheck_expression_binary bop lexp rexp =
  let tl = get_type lexp in
  let tr = get_type rexp in
  let convert lexp rexp =
    let tl = get_type lexp in
    let tr = get_type rexp in
    let common_t = get_common_type tl tr in
    convert_to common_t lexp, convert_to common_t rexp
  in
  match bop with
  | And | Or ->
    if is_scalar tl |> not || is_scalar tr |> not
    then raise (SemanticError "Logical operators can't have non-scalar expressions");
    Binary { bop; lexp; rexp; etp = Int }
  | Add ->
    if is_arithmetic_type tl && is_arithmetic_type tr
    then (
      let lexp, rexp = convert lexp rexp in
      Binary { bop; lexp; rexp; etp = get_type lexp })
    else if is_pointer_to_complete tl && is_integer_type tr
    then (
      let rexp = convert_to Long rexp in
      Binary { bop; lexp; rexp; etp = get_type lexp })
    else if is_integer_type tl && is_pointer_to_complete tr
    then (
      let lexp = convert_to Long lexp in
      Binary { bop; lexp; rexp; etp = get_type rexp })
    else raise (SemanticError "Invalid operands for addition")
  | Sub ->
    if is_arithmetic_type tl && is_arithmetic_type tr
    then (
      let lexp, rexp = convert lexp rexp in
      Binary { bop; lexp; rexp; etp = get_type lexp })
    else if is_pointer_to_complete tl && is_integer_type tr
    then (
      let rexp = convert_to Long rexp in
      Binary { bop; lexp; rexp; etp = get_type lexp })
    else if is_pointer_to_complete tl && tl = tr
    then Binary { bop; lexp; rexp; etp = Long }
    else raise (SemanticError "Invalid operands for subtraction")
  | Mul | Div ->
    if is_pointer_type tl || is_pointer_type tr
    then raise (SemanticError "Invalid binary operation on pointer type");
    let lexp, rexp = convert lexp rexp in
    Binary { bop; lexp; rexp; etp = get_type lexp }
  | Rem ->
    if tl = Double || tr = Double
    then raise (SemanticError "Can't use double with remainder operator");
    if is_pointer_type tl || is_pointer_type tr
    then raise (SemanticError "Invalid binary operation on pointer type");
    let lexp, rexp = convert lexp rexp in
    Binary { bop; lexp; rexp; etp = get_type lexp }
  | BAnd | BOr | Xor ->
    if is_pointer_type tl || is_pointer_type tr
    then raise (SemanticError "Can't use pointer with bitwise operator");
    if tl = Double || tr = Double
    then raise (SemanticError "Can't use double with bitwise operator");
    let lexp, rexp = convert lexp rexp in
    Binary { bop; lexp; rexp; etp = get_type lexp }
  | Equal | NEqual ->
    let common_t =
      if is_pointer_type tl || is_pointer_type tr
      then get_common_pointer_type lexp rexp
      else if is_arithmetic_type tl && is_arithmetic_type tr
      then get_common_type tl tr
      else raise (SemanticError "Invalid operands for equality expression")
    in
    let lexp = convert_to common_t lexp in
    let rexp = convert_to common_t rexp in
    Binary { bop; lexp; rexp; etp = Int }
  | LEqual | GEqual | Less | Greater ->
    if is_arithmetic_type tl && is_arithmetic_type tr
    then (
      let lexp, rexp = convert lexp rexp in
      Binary { bop; lexp; rexp; etp = Int })
    else if is_pointer_type tl && tl = tr
    then Binary { bop; lexp; rexp; etp = Int }
    else raise (SemanticError "Invalid operand for comparison operator")
  | Lsft | Rsft ->
    if tl = Double || tr = Double
    then raise (SemanticError "Can't use double with shift operator");
    if is_pointer_type tl || is_pointer_type tr
    then raise (SemanticError "Can't use pointer with shift operator");
    if is_scalar tr |> not
    then raise (SemanticError "Can't use non-scalar value with shift operator");
    let lexp = if is_char_type (get_type lexp) then convert_to Int lexp else lexp in
    Binary { bop; lexp; rexp; etp = get_type lexp }
;;

let rec typecheck_expression symbol_map = function
  | Constant (c, _) -> Constant (c, TypeConverter.const_type c)
  | CString (s, _) -> CString (s, CArray (Char, String.length s + 1))
  | Var (Identifier iden, _) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some SymbolMap.{ tp = FunType _; _ } ->
       raise (SemanticError ("Function name used as variable - " ^ iden))
     | Some SymbolMap.{ tp; _ } -> Var (Identifier iden, tp)
     | None -> assert false)
  | Cast { tgt; exp; _ } ->
    validate_type_specifier tgt;
    let exp = typecheck_expression_and_convert symbol_map exp in
    let etp = get_type exp in
    (match tgt with
     | Void -> Cast { tgt; exp; etp = tgt }
     | Double when is_pointer_type etp ->
       raise (SemanticError "Can't cast pointer to double")
     | Pointer _ when etp = Double -> raise (SemanticError "Can't cast double to pointer")
     | CArray _ -> raise (SemanticError "Can't cast to array type")
     | t when is_scalar t |> not ->
       raise (SemanticError "Can only cast to scalar type or void")
     | _ ->
       if is_scalar etp |> not
       then raise (SemanticError "Can't cast non-scalar expressions to scalar type");
       Cast { tgt; exp; etp = tgt })
  | Unary (uop, exp, _) ->
    let exp = typecheck_expression_and_convert symbol_map exp in
    let exp_t = get_type exp in
    if uop = Complement && exp_t = Double
    then raise (SemanticError "Can't take the bitwise complement of a double");
    if (uop = Complement || uop = Negate) && is_pointer_type exp_t
    then raise (SemanticError "Invalid unary operation on pointer type");
    if is_scalar exp_t |> not
    then raise (SemanticError "Logical operators only apply to scalar expressions");
    let exp =
      if (uop = Complement || uop = Negate) && is_char_type exp_t
      then convert_to Int exp
      else exp
    in
    let etp =
      match uop with
      | Not -> Int
      | _ -> get_type exp
    in
    Unary (uop, exp, etp)
  | TUnary (uop, p, lval, _) ->
    let lval = typecheck_expression_and_convert symbol_map lval in
    if is_lvalue lval |> not
    then raise (SemanticError "Invalid lvalue of suffix/postfix operator");
    if get_type lval = Pointer Void
    then raise (SemanticError "Can't use suffix/postfix operator on void pointers");
    TUnary (uop, p, lval, get_type lval)
  | Binary { bop; lexp; rexp; _ } ->
    let lexp = typecheck_expression_and_convert symbol_map lexp in
    let rexp = typecheck_expression_and_convert symbol_map rexp in
    typecheck_expression_binary bop lexp rexp
  | CompoundAssign { bop; lexp; rexp; _ } ->
    let lval = typecheck_expression_and_convert symbol_map lexp in
    let rexp = typecheck_expression_and_convert symbol_map rexp in
    let etp = get_type lval in
    if not (is_lvalue lval)
    then raise (SemanticError "Invalid lvalue of binary assignment operator");
    if is_pointer_type (get_type rexp)
    then raise (SemanticError "Invalid rhs for compound assignment");
    let tmp_binary_ins = typecheck_expression_binary bop lval rexp in
    let btp =
      match tmp_binary_ins with
      | Binary { etp; _ } -> etp
      | _ -> assert false
    in
    CompoundAssign { bop; lexp = lval; rexp; btp; etp }
  | Assignment { lval; rval; _ } ->
    let lval = typecheck_expression_and_convert symbol_map lval in
    if not (is_lvalue lval)
    then raise (SemanticError "Invalid lvalue of assignment operator");
    let etp = get_type lval in
    let rval = typecheck_expression_and_convert symbol_map rval in
    let rval = convert_by_assignment etp rval in
    Assignment { lval; rval; etp }
  | Conditional { cnd; lhs; rhs; _ } ->
    let cnd = typecheck_expression_and_convert symbol_map cnd in
    let lhs = typecheck_expression_and_convert symbol_map lhs in
    let rhs = typecheck_expression_and_convert symbol_map rhs in
    let tl = get_type lhs in
    let tr = get_type rhs in
    if get_type cnd |> is_scalar |> not
    then raise (SemanticError "Condition in conditional operator must be scalar");
    let common_t =
      if tl = Void && tr = Void
      then Void
      else if is_pointer_type tl || is_pointer_type tr
      then get_common_pointer_type lhs rhs
      else if is_arithmetic_type tl && is_arithmetic_type tr
      then get_common_type tl tr
      else raise (SemanticError "Cannot convert branches of conditional to a common type")
    in
    let lhs = convert_to common_t lhs in
    let rhs = convert_to common_t rhs in
    Conditional { cnd; lhs; rhs; etp = common_t }
  | FunctionCall (Identifier iden, exps, _) ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some SymbolMap.{ tp = FunType { params; ret }; _ } ->
       if List.length params <> List.length exps
       then
         raise
           (SemanticError ("Function called with wrong number of arguments - " ^ iden));
       let exps = List.map (typecheck_expression_and_convert symbol_map) exps in
       let converted_args = List.map2 convert_by_assignment params exps in
       FunctionCall (Identifier iden, converted_args, ret)
     | _ -> raise (SemanticError ("Variable used as function - " ^ iden)))
  | Dereference (exp, _) ->
    let exp = typecheck_expression_and_convert symbol_map exp in
    (match get_type exp with
     | Pointer ptp when ptp = Void ->
       raise (SemanticError "Can't dereference void pointer")
     | Pointer ptp -> Dereference (exp, ptp)
     | _ -> raise (SemanticError "Dereferencing a non-pointer type"))
  | AddrOf (exp, _) ->
    if not (is_lvalue exp) then raise (SemanticError "Taking address of non-lvalue");
    let exp = typecheck_expression symbol_map exp in
    let etp = get_type exp in
    AddrOf (exp, Pointer etp)
  | Subscript (e1, e2, _) ->
    let e1 = typecheck_expression_and_convert symbol_map e1 in
    let e2 = typecheck_expression_and_convert symbol_map e2 in
    let t1 = get_type e1 in
    let t2 = get_type e2 in
    let ptr, e1, e2 =
      if is_pointer_to_complete t1 && is_integer_type t2
      then t1, e1, convert_to Long e2
      else if is_integer_type t1 && is_pointer_to_complete t2
      then t2, convert_to Long e1, e2
      else raise (SemanticError "Subscript must have integer and pointer operand")
    in
    let ptr_ref =
      match ptr with
      | Pointer tp -> tp
      | _ -> assert false
    in
    Subscript (e1, e2, ptr_ref)
  | SizeOf (exp, _) ->
    let exp = typecheck_expression symbol_map exp in
    if get_type exp |> is_complete |> not then raise (SemanticError "");
    SizeOf (exp, ULong)
  | SizeOfT (tp, _) ->
    validate_type_specifier tp;
    if is_complete tp |> not
    then raise (SemanticError "Can't get the size of an incomplete type");
    SizeOfT (tp, ULong)
  | Dot _ | Arrow _ -> assert false

and typecheck_expression_and_convert symbol_map exp =
  let exp = typecheck_expression symbol_map exp in
  match get_type exp with
  | CArray (tp, _) -> AddrOf (exp, Pointer tp)
  | _ -> exp
;;

let rec typecheck_static_init vtp = function
  | SingleInit (CString (s, _), _) ->
    (match vtp with
     | CArray (tp, sz) ->
       let s_len = String.length s in
       if is_char_type tp |> not
       then
         raise (SemanticError "Can't initialize a non-character type with string literal");
       if s_len > sz then raise (SemanticError "Too many characters in string literal");
       let null_terminated = s_len < sz in
       let zero_bytes = sz - s_len - 1 in
       if zero_bytes > 0
       then [ StringInit { str = s; null_terminated }; ZeroInit { bytes = zero_bytes } ]
       else [ StringInit { str = s; null_terminated } ]
     | Pointer tp ->
       if tp <> Char
       then
         raise (SemanticError "Can't initialize a non-character type with string literal");
       let name = StaticStringMap.get_const_string_label s in
       [ PointerInit { name } ]
     | _ ->
       raise (SemanticError "Can't initialize a non-character type with string literal"))
  | SingleInit (Constant (c, _), _) ->
    let c =
      match vtp with
      | Char | SChar -> CharInit (TypeConverter.convert_to_char c)
      | UChar -> UCharInit (TypeConverter.convert_to_uchar c)
      | Int -> IntInit (TypeConverter.convert_to_int c)
      | UInt -> UIntInit (TypeConverter.convert_to_uint c)
      | Long -> LongInit (TypeConverter.convert_to_long c)
      | ULong -> ULongInit (TypeConverter.convert_to_ulong c)
      | Double -> DoubleInit (TypeConverter.convert_to_double c)
      | Void -> assert false
      | FunType _ -> assert false
      | Pointer _ ->
        let v = TypeConverter.convert_to_long c in
        if v <> 0L
        then
          raise
            (SemanticError "Non-null pointer constant used to initialize pointer variable")
        else ULongInit 0I
      | CArray _ -> raise (SemanticError "Invalid static initializer for arrays")
      | Structure _ -> assert false
    in
    [ c ]
  | CompoundInit (init_list, _) ->
    (match vtp with
     | CArray (tp, sz) ->
       if List.length init_list > sz
       then raise (SemanticError "Wrong number of values in initializer");
       let tail_sz = (sz - List.length init_list) * size tp in
       let tail = if tail_sz > 0 then [ ZeroInit { bytes = tail_sz } ] else [] in
       let init_list = List.concat_map (typecheck_static_init tp) init_list in
       init_list @ tail
     | _ -> raise (SemanticError "Invalid static initializer for arrays"))
  | _ -> raise (SemanticError "Non-constant initializer for file-scope variable")
;;

let typecheck_file_scope_variable_decl symbol_map = function
  | { name = Identifier iden; init; vtp; storage } as ret ->
    validate_type_specifier vtp;
    if vtp = Void then raise (SemanticError "Can't declare void variables");
    let initial_value =
      ref
        (match init with
         | Some ci -> SymbolMap.Initial (typecheck_static_init vtp ci)
         | None ->
           if storage = Some Extern then SymbolMap.NoInitial else SymbolMap.Tentative)
    in
    let global = ref (storage <> Some Static) in
    (match Hashtbl.find_opt symbol_map iden with
     | Some SymbolMap.{ tp; attrs } ->
       if tp <> vtp
       then raise (SemanticError ("Variable declaration type mismatch - " ^ iden));
       (match attrs with
        | StaticAttr attrs ->
          if storage = Some Extern
          then global := attrs.global
          else if attrs.global <> !global
          then raise (SemanticError ("Conflicting variable linkage - " ^ iden));
          let is_constant init =
            match init with
            | SymbolMap.Initial _ -> true
            | _ -> false
          in
          if is_constant attrs.init
          then
            if is_constant !initial_value
            then raise (SemanticError ("Redefinition of constant variable - " ^ iden))
            else initial_value := attrs.init
          else if (not (is_constant !initial_value)) && attrs.init = SymbolMap.Tentative
          then initial_value := SymbolMap.Tentative
        | _ -> assert false)
     | _ -> ());
    let attrs = SymbolMap.StaticAttr { init = !initial_value; global = !global } in
    let info = SymbolMap.{ tp = vtp; attrs } in
    Hashtbl.replace symbol_map iden info;
    ret
;;

let rec typecheck_var_init symbol_map tgt init =
  match tgt, init with
  | CArray (tp, sz), SingleInit (CString (s, _), _) ->
    if is_char_type tp |> not
    then raise (SemanticError "Can't initialize a non-character type with string literal");
    if String.length s > sz
    then raise (SemanticError "Too many characters in string literal");
    SingleInit (CString (s, tgt), tgt)
  | _, SingleInit (exp, _) ->
    let exp = typecheck_expression_and_convert symbol_map exp in
    let exp = convert_by_assignment tgt exp in
    SingleInit (exp, tgt)
  | CArray (tp, sz), CompoundInit (init_list, _) ->
    if List.length init_list > sz
    then raise (SemanticError "Wrong number of values in initializer");
    let init_list = List.map (fun el -> typecheck_var_init symbol_map tp el) init_list in
    let padding = List.init (sz - List.length init_list) (fun _ -> zero_initializer tp) in
    CompoundInit (init_list @ padding, tgt)
  | _ ->
    raise (SemanticError "Can't initialize a scalar object with compound initializer")
;;

let typecheck_block_scope_variable_decl symbol_map = function
  | { name = Identifier iden; init; vtp; storage } as ret ->
    validate_type_specifier vtp;
    if vtp = Void then raise (SemanticError "Can't declare void variables");
    (match storage with
     | Some Extern ->
       if init <> None
       then
         raise
           (SemanticError ("Initializer for local extern variable declaration - " ^ iden));
       (match Hashtbl.find_opt symbol_map iden with
        | Some SymbolMap.{ tp; _ } ->
          if tp <> vtp
          then raise (SemanticError ("Variable declaration type mismatch - " ^ iden))
        | None ->
          let info =
            SymbolMap.
              { tp = vtp
              ; attrs = StaticAttr { init = SymbolMap.NoInitial; global = true }
              }
          in
          Hashtbl.replace symbol_map iden info);
       ret
     | Some Static ->
       let initial_value =
         match init with
         | Some ci -> SymbolMap.Initial (typecheck_static_init vtp ci)
         | None -> SymbolMap.Tentative
       in
       let info =
         SymbolMap.
           { tp = vtp; attrs = StaticAttr { init = initial_value; global = false } }
       in
       Hashtbl.replace symbol_map iden info;
       ret
     | None ->
       let info = SymbolMap.{ tp = vtp; attrs = LocalAttr } in
       Hashtbl.replace symbol_map iden info;
       let init = Option.map (typecheck_var_init symbol_map vtp) init in
       { name = Identifier iden; init; vtp; storage })
;;

let typecheck_for_init symbol_map = function
  | InitDecl d ->
    if d.storage <> None then raise (SemanticError "Storage specifier in for-loop init");
    InitDecl (typecheck_block_scope_variable_decl symbol_map d)
  | InitExp e -> InitExp (Option.map (typecheck_expression_and_convert symbol_map) e)
;;

(* ftp - enclosing function return type. *)
let rec typecheck_statement symbol_map ftp = function
  | Return exp ->
    if ftp = Void && exp <> None
    then raise (SemanticError "Can't return value with void return type");
    if ftp <> Void && exp = None
    then raise (SemanticError "Not returning a value with non-void return type");
    let exp = Option.map (typecheck_expression_and_convert symbol_map) exp in
    let exp = Option.map (convert_by_assignment ftp) exp in
    Return exp
  | Expression exp -> Expression (typecheck_expression_and_convert symbol_map exp)
  | If { cnd; thn; els } ->
    let cnd = typecheck_expression_and_convert symbol_map cnd in
    let thn = typecheck_statement symbol_map ftp thn in
    let els = Option.map (typecheck_statement symbol_map ftp) els in
    if get_type cnd |> is_scalar |> not
    then raise (SemanticError "Non-scalar type in if condition");
    If { cnd; thn; els }
  | Label (lbl, stmt) -> Label (lbl, typecheck_statement symbol_map ftp stmt)
  | Compound block -> Compound (typecheck_block symbol_map ftp block)
  | While (exp, stmt, lbl) ->
    let exp = typecheck_expression_and_convert symbol_map exp in
    let stmt = typecheck_statement symbol_map ftp stmt in
    if get_type exp |> is_scalar |> not
    then raise (SemanticError "Non-scalar type in while condition");
    While (exp, stmt, lbl)
  | DoWhile (stmt, exp, lbl) ->
    let stmt = typecheck_statement symbol_map ftp stmt in
    let exp = typecheck_expression_and_convert symbol_map exp in
    if get_type exp |> is_scalar |> not
    then raise (SemanticError "Non-scalar type in do-while condition");
    DoWhile (stmt, exp, lbl)
  | For { init; cnd; post; body; label } ->
    let init = typecheck_for_init symbol_map init in
    let cnd = Option.map (typecheck_expression_and_convert symbol_map) cnd in
    let post = Option.map (typecheck_expression_and_convert symbol_map) post in
    let body = typecheck_statement symbol_map ftp body in
    if cnd <> None && Option.get cnd |> get_type |> is_scalar |> not
    then raise (SemanticError "Non-scalar type in while condition");
    For { init; cnd; post; body; label }
  | Switch { cnd; body; cases; default; label = Identifier label } ->
    let cnd = typecheck_expression symbol_map cnd in
    if is_array_type (get_type cnd)
    then raise (SemanticError "Array values can't be used in switch condition");
    if get_type cnd = Double then raise (SemanticError "Double value in switch condition");
    add_for_label label (get_type cnd);
    if get_type cnd |> is_scalar |> not
    then raise (SemanticError "Non-scalar values can't be used in switch condition");
    let body = typecheck_statement symbol_map ftp body in
    let cnd =
      match get_type cnd with
      | Char | SChar -> convert_to Int cnd
      | UChar -> convert_to UInt cnd
      | _ -> cnd
    in
    Switch { cnd; body; cases; default; label = Identifier label }
  | Case (exp, stmt, Identifier lbl) ->
    let exp = typecheck_expression_and_convert symbol_map exp in
    if get_type exp = Double then raise (SemanticError "Double value in switch-case");
    let etp = get_for_label lbl in
    let exp = convert_to etp exp in
    let stmt = typecheck_statement symbol_map ftp stmt in
    Case (exp, stmt, Identifier lbl)
  | Default (stmt, lbl) ->
    let stmt = typecheck_statement symbol_map ftp stmt in
    Default (stmt, lbl)
  | (Goto _ | Break _ | Continue _ | Null) as ret -> ret

and typecheck_block_item symbol_map ftp = function
  | S s -> S (typecheck_statement symbol_map ftp s)
  | D d -> D (typecheck_declaration symbol_map true d)

and typecheck_block symbol_map ftp = function
  | Block items ->
    let items = List.map (typecheck_block_item symbol_map ftp) items in
    Block items

and typecheck_function_decl symbol_map = function
  | { name = Identifier iden; params; body; ftp; storage } ->
    validate_type_specifier ftp;
    let has_body = Option.is_some body in
    let already_defined = ref false in
    let global = ref (storage <> Some Static) in
    let ptps, rtp =
      match ftp with
      | FunType { params; ret } -> params, ret
      | _ -> assert false
    in
    if is_array_type rtp then raise (SemanticError "A function can't return an array");
    let adjust_param = function
      | CArray (tp, _) -> Pointer tp
      | x -> x
    in
    let ptps = List.map adjust_param ptps in
    let ftp = FunType { params = ptps; ret = rtp } in
    List.iter
      (function
        | Void -> raise (SemanticError "Can't declare void variables")
        | _ -> ())
      ptps;
    (match Hashtbl.find_opt symbol_map iden with
     | Some SymbolMap.{ tp; attrs } ->
       if ftp <> tp
       then raise (SemanticError ("Incomplete function declaration - " ^ iden));
       (match attrs with
        | SymbolMap.FunAttr attrs ->
          already_defined := attrs.defined;
          if !already_defined && has_body
          then raise (SemanticError ("Function is defined more thand once - " ^ iden));
          if attrs.global && storage = Some Static
          then
            raise
              (SemanticError ("Static function declaration follows non-static - " ^ iden));
          global := attrs.global
        | _ -> assert false)
     | _ -> ());
    let attrs =
      SymbolMap.FunAttr { defined = !already_defined || has_body; global = !global }
    in
    let info = SymbolMap.{ tp = ftp; attrs } in
    Hashtbl.replace symbol_map iden info;
    let typecheck_param symbol_map ptp = function
      | Identifier name ->
        Hashtbl.replace symbol_map name SymbolMap.{ tp = ptp; attrs = LocalAttr }
    in
    let _ = List.map2 (typecheck_param symbol_map) ptps params in
    let body = Option.map (typecheck_block symbol_map rtp) body in
    { name = Identifier iden; params; body; ftp; storage }

and typecheck_declaration symbol_map nested = function
  | FunDecl f -> FunDecl (typecheck_function_decl symbol_map f)
  | VarDecl v ->
    if nested
    then VarDecl (typecheck_block_scope_variable_decl symbol_map v)
    else VarDecl (typecheck_file_scope_variable_decl symbol_map v)
  | StructDecl _ -> assert false
;;

let typecheck_program = function
  | Program dns ->
    Program (List.map (typecheck_declaration SymbolMap.symbol_map false) dns)
;;
