exception SyntaxError = Errors.SyntaxError

type specifier =
  | IntSpec
  | LongSpec
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
  | Eq -> C_ast.Assignment { lval; rval; etp = C_ast.Int }
  | AEq | SEq | MEq | DEq | REq | BAEq | BOEq | XEq | LsftEq | RsftEq ->
    let bop = convert_aop_to_bop aop in
    let bin = C_ast.Binary { bop; lexp = lval; rexp = rval; etp = C_ast.Int } in
    C_ast.Assignment { lval; rval = bin; etp = C_ast.Int }
;;

let process_specs specs =
  let idx_of = function
    | IntSpec -> 0
    | LongSpec -> 1
    | SignedSpec -> 2
    | UnsignedSpec -> 3
    | StaticSpec -> 4
    | ExternSpec -> 5
  in
  let cnt = Array.make 6 0 in
  let f spec = cnt.(idx_of spec) <- cnt.(idx_of spec) + 1 in
  List.iter f specs;
  if not (Array.for_all (fun x -> x <= 1) cnt)
  then raise (SyntaxError "Multiple specifiers");
  if cnt.(0) + cnt.(1) + cnt.(2) + cnt.(3) = 0
  then raise (SyntaxError "No type specifier");
  if cnt.(2) + cnt.(3) > 1 then raise (SyntaxError "Multiple sign specifiers");
  if cnt.(4) + cnt.(5) > 1 then raise (SyntaxError "Multiple storage specifiers");
  cnt
;;

let storage specs =
  let cnt = process_specs specs in
  if cnt.(4) = 1
  then Some C_ast.Static
  else if cnt.(5) = 1
  then Some C_ast.Extern
  else None
;;

let type_of specs =
  let cnt = process_specs specs in
  if cnt.(3) + cnt.(1) = 2
  then C_ast.ULong
  else if cnt.(3) = 1
  then C_ast.UInt
  else if cnt.(1) = 1
  then C_ast.Long
  else C_ast.Int
;;
