open Common

exception SyntaxError = Errors.SyntaxError

type specifier =
  | IntSpec
  | LongSpec
  | DoubleSpec
  | SignedSpec
  | UnsignedSpec
  | StaticSpec
  | ExternSpec

type assign_op =
  | Eq
  | AEq
  | SEq
  | MEq
  | DEq
  | REq
  | BAEq
  | BOEq
  | XEq
  | LsftEq
  | RsftEq

type param_info = Param of c_type * declarator [@@deriving show]

and declarator =
  | Ident of identifier
  | PointerDeclarator of declarator
  | ArrayDeclarator of declarator * const
  | FunDeclarator of param_info list * declarator
[@@deriving show]

type abstract_declarator =
  | AbstractPointer of abstract_declarator
  | AbstractArray of abstract_declarator * const
  | AbstractBase
[@@deriving show]

let convert_aop_to_bop = function
  | Eq -> assert false
  | AEq -> C_ast.Add
  | SEq -> C_ast.Sub
  | MEq -> C_ast.Mul
  | DEq -> C_ast.Div
  | REq -> C_ast.Rem
  | BAEq -> C_ast.BAnd
  | BOEq -> C_ast.BOr
  | XEq -> C_ast.Xor
  | LsftEq -> C_ast.Lsft
  | RsftEq -> C_ast.Rsft
;;

let assignment_ast aop lval rval =
  match aop with
  | Eq -> C_ast.Assignment { lval; rval; etp = Int }
  | AEq | SEq | MEq | DEq | REq | BAEq | BOEq | XEq | LsftEq | RsftEq ->
    let bop = convert_aop_to_bop aop in
    C_ast.CompoundAssign { bop; lexp = lval; rexp = rval; btp = Int; etp = Int }
;;

let process_specs specs =
  let idx_of = function
    | IntSpec -> 0
    | LongSpec -> 1
    | DoubleSpec -> 2
    | SignedSpec -> 3
    | UnsignedSpec -> 4
    | StaticSpec -> 5
    | ExternSpec -> 6
  in
  let cnt = Array.make 7 0 in
  let f spec = cnt.(idx_of spec) <- cnt.(idx_of spec) + 1 in
  List.iter f specs;
  if not (Array.for_all (fun x -> x <= 1) cnt)
  then raise (SyntaxError "Multiple specifiers");
  if cnt.(0) + cnt.(1) + cnt.(2) + cnt.(3) + cnt.(4) = 0
  then raise (SyntaxError "No type specifier");
  if cnt.(2) = 1 && cnt.(0) + cnt.(1) + cnt.(2) + cnt.(3) + cnt.(4) > 1
  then raise (SyntaxError "Invalid specifier for double");
  if cnt.(3) + cnt.(4) > 1 then raise (SyntaxError "Multiple sign specifiers");
  if cnt.(5) + cnt.(6) > 1 then raise (SyntaxError "Multiple storage specifiers");
  cnt
;;

let storage specs =
  let cnt = process_specs specs in
  if cnt.(5) = 1
  then Some C_ast.Static
  else if cnt.(6) = 1
  then Some C_ast.Extern
  else None
;;

let type_of specs =
  let cnt = process_specs specs in
  if cnt.(2) = 1
  then Double
  else if cnt.(4) + cnt.(1) = 2
  then ULong
  else if cnt.(4) = 1
  then UInt
  else if cnt.(1) = 1
  then Long
  else Int
;;

let array_size = function
  | ConstDouble _ -> raise (SyntaxError "Invalid array size")
  | _ as ret -> Int64.to_int (Type_converter.convert_to_long ret)
;;

let rec process_declarator base_tp = function
  | Ident name -> name, base_tp, []
  | PointerDeclarator d ->
    let derived_tp = Pointer base_tp in
    process_declarator derived_tp d
  | ArrayDeclarator (inner, sz) ->
    let derived_tp = CArray (base_tp, array_size sz) in
    process_declarator derived_tp inner
  | FunDeclarator (params, d) ->
    (match d with
     | Ident name ->
       let param_types = ref [] in
       let param_names = ref [] in
       List.iter
         (fun (Param (p_base_tp, pd)) ->
            let pname, ptp, _ = process_declarator p_base_tp pd in
            (match ptp with
             | FunType _ ->
               raise (SyntaxError "Function pointers in parameters aren't supported.")
             | _ -> ());
            param_types := ptp :: !param_types;
            param_names := pname :: !param_names)
         params;
       let derived_tp = FunType { params = List.rev !param_types; ret = base_tp } in
       name, derived_tp, List.rev !param_names
     | _ ->
       raise (SyntaxError "Can't apply additional type deriviation to a function type."))
;;

let rec process_abstract_declarator base_tp = function
  | AbstractBase -> base_tp
  | AbstractPointer ad ->
    let derived_tp = Pointer base_tp in
    process_abstract_declarator derived_tp ad
  | AbstractArray (ad, sz) ->
    let derived_tp = CArray (base_tp, array_size sz) in
    process_abstract_declarator derived_tp ad
;;

let function_decl_ast specs d body =
  let rtp = type_of specs in
  let name, ftp, params = process_declarator rtp d in
  (match ftp with
   | FunType _ -> ()
   | _ ->
     print_endline (show_c_type ftp);
     raise (SyntaxError "Function declarator does not yield function type"));
  C_ast.{ name; params; body; ftp; storage = storage specs }
;;

let parse_declaration specs d =
  let tp = type_of specs in
  let name, dtp, params = process_declarator tp d in
  match dtp with
  | FunType _ ->
    C_ast.FunDecl { name; params; body = None; ftp = dtp; storage = storage specs }
  | _ -> C_ast.VarDecl { name; init = None; vtp = dtp; storage = storage specs }
;;
