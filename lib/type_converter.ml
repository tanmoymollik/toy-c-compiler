open Stdint
open Common

let const_type = function
  | ConstInt _ -> Int
  | ConstUInt _ -> UInt
  | ConstLong _ -> Long
  | ConstULong _ -> ULong
  | ConstDouble _ -> Double
;;

(* Converts to c int which is 32 bits wide. *)
let convert_to_int = function
  | ConstInt i -> i
  | ConstUInt ui -> Int32.of_uint32 ui
  | ConstLong l -> Int32.of_int64 l
  | ConstULong ul -> Int32.of_uint64 ul
  | ConstDouble d -> Int32.of_float d
;;

(* Converts to c unsigned int which is 32 bits wide. *)
let convert_to_uint = function
  | ConstInt i -> Uint32.of_int32 i
  | ConstUInt ui -> ui
  | ConstLong l -> Uint32.of_int64 l
  | ConstULong ul -> Uint32.of_uint64 ul
  | ConstDouble d -> Uint32.of_float d
;;

(* Converts to c long which is 64 bits wide. *)
let convert_to_long = function
  | ConstInt i -> Int64.of_int32 i
  | ConstUInt ui -> Int64.of_uint32 ui
  | ConstLong l -> l
  | ConstULong ul -> Int64.of_uint64 ul
  | ConstDouble d -> Int64.of_float d
;;

(* Converts to c unsigned long which is 64 bits wide. *)
let convert_to_ulong = function
  | ConstInt i -> Uint64.of_int32 i
  | ConstUInt ui -> Uint64.of_uint32 ui
  | ConstLong l -> Uint64.of_int64 l
  | ConstULong ul -> ul
  | ConstDouble d -> Uint64.of_float d
;;

(* Converts to c double which is 64 bits wide. *)
let convert_to_double = function
  | ConstInt i -> Int32.to_float i
  | ConstUInt ui -> Uint32.to_float ui
  | ConstLong l -> Int64.to_float l
  | ConstULong ul -> Uint64.to_float ul
  | ConstDouble d -> d
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
  | Int ->
    ConstInt (evaluate_int32_binary_expression bop (convert_to_int l) (convert_to_int r))
  | UInt ->
    ConstUInt
      (evaluate_uint32_binary_expression bop (convert_to_uint l) (convert_to_uint r))
  | Long ->
    ConstLong
      (evaluate_int64_binary_expression bop (convert_to_long l) (convert_to_long r))
  | ULong ->
    ConstULong
      (evaluate_uint64_binary_expression bop (convert_to_ulong l) (convert_to_ulong r))
  | Double -> assert false
  | FunType _ -> assert false
  | Pointer _ -> assert false
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
  | Int -> ConstInt (evaluate_int32_unary_expression uop (convert_to_int x))
  | UInt -> ConstUInt (evaluate_uint32_unary_expression uop (convert_to_uint x))
  | Long -> ConstLong (evaluate_int64_unary_expression uop (convert_to_long x))
  | ULong -> ConstULong (evaluate_uint64_unary_expression uop (convert_to_ulong x))
  | Double -> assert false
  | FunType _ -> assert false
  | Pointer _ -> assert false
;;

let evaluate_conditional_expression cnd lhs rhs =
  assert (const_type lhs = const_type rhs);
  let ctp = const_type cnd in
  let zr = c_type_zero ctp in
  if cnd <> zr then lhs else rhs
;;
