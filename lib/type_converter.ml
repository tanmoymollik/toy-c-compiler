open Stdint

let const_type = function
  | Common.ConstInt _ -> C_ast.Int
  | Common.ConstUInt _ -> C_ast.UInt
  | Common.ConstLong _ -> C_ast.Long
  | Common.ConstULong _ -> C_ast.ULong
;;

(* Converts to c int which is 32 bits wide. *)
let convert_to_int = function
  | Common.ConstInt i -> i
  | Common.ConstUInt ui -> Int32.of_uint32 ui
  | Common.ConstLong l -> Int32.of_int64 l
  | Common.ConstULong ul -> Int32.of_uint64 ul
;;

(* Converts to c unsigned int which is 32 bits wide. *)
let convert_to_uint = function
  | Common.ConstInt i -> Uint32.of_int32 i
  | Common.ConstUInt ui -> ui
  | Common.ConstLong l -> Uint32.of_int64 l
  | Common.ConstULong ul -> Uint32.of_uint64 ul
;;

(* Converts to c long which is 64 bits wide. *)
let convert_to_long = function
  | Common.ConstInt i -> Int64.of_int32 i
  | Common.ConstUInt ui -> Int64.of_uint32 ui
  | Common.ConstLong l -> l
  | Common.ConstULong ul -> Int64.of_uint64 ul
;;

(* Converts to c unsigned long which is 64 bits wide. *)
let convert_to_ulong = function
  | Common.ConstInt i -> Uint64.of_int32 i
  | Common.ConstUInt ui -> Uint64.of_uint32 ui
  | Common.ConstLong l -> Uint64.of_int64 l
  | Common.ConstULong ul -> ul
;;

let evaluate_int32_binary_expression bop l r =
  match bop with
  | C_ast.Add -> Int32.add l r
  | C_ast.Sub -> Int32.sub l r
  | C_ast.Mul -> Int32.mul l r
  | C_ast.Div -> Int32.div l r
  | C_ast.Rem -> Int32.rem l r
  | C_ast.BAnd -> Int32.logand l r
  | C_ast.BOr -> Int32.logor l r
  | C_ast.Xor -> Int32.logxor l r
  | C_ast.Lsft -> Int32.shift_left l (Int32.to_int r)
  | C_ast.Rsft -> Int32.shift_right l (Int32.to_int r)
  | C_ast.And -> if l != 0l && r != 0l then 1l else 0l
  | C_ast.Or -> if l != 0l || r != 0l then 1l else 0l
  | C_ast.Equal -> if l = r then 1l else 0l
  | C_ast.NEqual -> if l <> r then 1l else 0l
  | C_ast.LEqual -> if l <= r then 1l else 0l
  | C_ast.GEqual -> if l >= r then 1l else 0l
  | C_ast.Less -> if l < r then 1l else 0l
  | C_ast.Greater -> if l > r then 1l else 0l
;;

let evaluate_uint32_binary_expression bop l r =
  match bop with
  | C_ast.Add -> Uint32.add l r
  | C_ast.Sub -> Uint32.sub l r
  | C_ast.Mul -> Uint32.mul l r
  | C_ast.Div -> Uint32.div l r
  | C_ast.Rem -> Uint32.rem l r
  | C_ast.BAnd -> Uint32.logand l r
  | C_ast.BOr -> Uint32.logor l r
  | C_ast.Xor -> Uint32.logxor l r
  | C_ast.Lsft -> Uint32.shift_left l (Uint32.to_int r)
  | C_ast.Rsft -> Uint32.shift_right l (Uint32.to_int r)
  | C_ast.And -> if l != 0i && r != 0i then 1i else 0i
  | C_ast.Or -> if l != 0i || r != 0i then 1i else 0i
  | C_ast.Equal -> if l = r then 1i else 0i
  | C_ast.NEqual -> if l <> r then 1i else 0i
  | C_ast.LEqual -> if l <= r then 1i else 0i
  | C_ast.GEqual -> if l >= r then 1i else 0i
  | C_ast.Less -> if l < r then 1i else 0i
  | C_ast.Greater -> if l > r then 1i else 0i
;;

let evaluate_int64_binary_expression bop l r =
  match bop with
  | C_ast.Add -> Int64.add l r
  | C_ast.Sub -> Int64.sub l r
  | C_ast.Mul -> Int64.mul l r
  | C_ast.Div -> Int64.div l r
  | C_ast.Rem -> Int64.rem l r
  | C_ast.BAnd -> Int64.logand l r
  | C_ast.BOr -> Int64.logor l r
  | C_ast.Xor -> Int64.logxor l r
  | C_ast.Lsft -> Int64.shift_left l (Int64.to_int r)
  | C_ast.Rsft -> Int64.shift_right l (Int64.to_int r)
  | C_ast.And -> if l != 0L && r != 0L then 1L else 0L
  | C_ast.Or -> if l != 0L || r != 0L then 1L else 0L
  | C_ast.Equal -> if l = r then 1L else 0L
  | C_ast.NEqual -> if l <> r then 1L else 0L
  | C_ast.LEqual -> if l <= r then 1L else 0L
  | C_ast.GEqual -> if l >= r then 1L else 0L
  | C_ast.Less -> if l < r then 1L else 0L
  | C_ast.Greater -> if l > r then 1L else 0L
;;

let evaluate_uint64_binary_expression bop l r =
  match bop with
  | C_ast.Add -> Uint64.add l r
  | C_ast.Sub -> Uint64.sub l r
  | C_ast.Mul -> Uint64.mul l r
  | C_ast.Div -> Uint64.div l r
  | C_ast.Rem -> Uint64.rem l r
  | C_ast.BAnd -> Uint64.logand l r
  | C_ast.BOr -> Uint64.logor l r
  | C_ast.Xor -> Uint64.logxor l r
  | C_ast.Lsft -> Uint64.shift_left l (Uint64.to_int r)
  | C_ast.Rsft -> Uint64.shift_right l (Uint64.to_int r)
  | C_ast.And -> if l != 0I && r != 0I then 1I else 0I
  | C_ast.Or -> if l != 0I || r != 0I then 1I else 0I
  | C_ast.Equal -> if l = r then 1I else 0I
  | C_ast.NEqual -> if l <> r then 1I else 0I
  | C_ast.LEqual -> if l <= r then 1I else 0I
  | C_ast.GEqual -> if l >= r then 1I else 0I
  | C_ast.Less -> if l < r then 1I else 0I
  | C_ast.Greater -> if l > r then 1I else 0I
;;

let evaluate_binary_expression bop l r =
  assert (const_type l = const_type r);
  let ctp = const_type l in
  match ctp with
  | C_ast.Int ->
    Common.ConstInt
      (evaluate_int32_binary_expression bop (convert_to_int l) (convert_to_int r))
  | C_ast.UInt ->
    Common.ConstUInt
      (evaluate_uint32_binary_expression bop (convert_to_uint l) (convert_to_uint r))
  | C_ast.Long ->
    Common.ConstLong
      (evaluate_int64_binary_expression bop (convert_to_long l) (convert_to_long r))
  | C_ast.ULong ->
    Common.ConstULong
      (evaluate_uint64_binary_expression bop (convert_to_ulong l) (convert_to_ulong r))
  | C_ast.FunType _ -> assert false
;;

let evaluate_int32_unary_expression uop x =
  match uop with
  | C_ast.Complement -> Int32.lognot x
  | C_ast.Negate -> Int32.neg x
  | C_ast.Not -> if x = 0l then 1l else 0l
;;

let evaluate_uint32_unary_expression uop x =
  match uop with
  | C_ast.Complement -> Uint32.lognot x
  | C_ast.Negate -> Uint32.neg x
  | C_ast.Not -> if x = 0i then 1i else 0i
;;

let evaluate_int64_unary_expression uop x =
  match uop with
  | C_ast.Complement -> Int64.lognot x
  | C_ast.Negate -> Int64.neg x
  | C_ast.Not -> if x = 0L then 1L else 0L
;;

let evaluate_uint64_unary_expression uop x =
  match uop with
  | C_ast.Complement -> Uint64.lognot x
  | C_ast.Negate -> Uint64.neg x
  | C_ast.Not -> if x = 0I then 1I else 0I
;;

let evaluate_unary_expression uop x =
  let ctp = const_type x in
  match ctp with
  | C_ast.Int -> Common.ConstInt (evaluate_int32_unary_expression uop (convert_to_int x))
  | C_ast.UInt ->
    Common.ConstUInt (evaluate_uint32_unary_expression uop (convert_to_uint x))
  | C_ast.Long ->
    Common.ConstLong (evaluate_int64_unary_expression uop (convert_to_long x))
  | C_ast.ULong ->
    Common.ConstULong (evaluate_uint64_unary_expression uop (convert_to_ulong x))
  | C_ast.FunType _ -> assert false
;;

let evaluate_conditional_expression cnd lhs rhs =
  assert (const_type lhs = const_type rhs);
  let ctp = const_type cnd in
  let zr =
    match ctp with
    | C_ast.Int -> Common.ConstInt 0l
    | C_ast.UInt -> Common.ConstUInt 0i
    | C_ast.Long -> Common.ConstLong 0L
    | C_ast.ULong -> Common.ConstULong 0I
    | C_ast.FunType _ -> assert false
  in
  if cnd <> zr then lhs else rhs
;;
