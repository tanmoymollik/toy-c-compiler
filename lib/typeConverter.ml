open Stdint
open Common

let const_type = function
  | ConstChar _ -> Char
  | ConstUChar _ -> UChar
  | ConstInt _ -> Int
  | ConstUInt _ -> UInt
  | ConstLong _ -> Long
  | ConstULong _ -> ULong
  | ConstDouble _ -> Double
;;

(* Converts to c int which is 32 bits wide. *)
let convert_to_int = function
  | ConstInt i | ConstChar i | ConstUChar i -> i
  | ConstUInt ui -> Int32.of_uint32 ui
  | ConstLong l -> Int32.of_int64 l
  | ConstULong ul -> Int32.of_uint64 ul
  | ConstDouble d -> Int32.of_float d
;;

(* Converts to c unsigned int which is 32 bits wide. *)
let convert_to_uint = function
  | ConstInt i | ConstChar i | ConstUChar i -> Uint32.of_int32 i
  | ConstUInt ui -> ui
  | ConstLong l -> Uint32.of_int64 l
  | ConstULong ul -> Uint32.of_uint64 ul
  | ConstDouble d -> Uint32.of_float d
;;

(* Converts to c long which is 64 bits wide. *)
let convert_to_long = function
  | ConstInt i | ConstChar i | ConstUChar i -> Int64.of_int32 i
  | ConstUInt ui -> Int64.of_uint32 ui
  | ConstLong l -> l
  | ConstULong ul -> Int64.of_uint64 ul
  | ConstDouble d -> Int64.of_float d
;;

(* Converts to c unsigned long which is 64 bits wide. *)
let convert_to_ulong = function
  | ConstInt i | ConstChar i | ConstUChar i -> Uint64.of_int32 i
  | ConstUInt ui -> Uint64.of_uint32 ui
  | ConstLong l -> Uint64.of_int64 l
  | ConstULong ul -> ul
  | ConstDouble d -> Uint64.of_float d
;;

(* Converts to c double which is 64 bits wide. *)
let convert_to_double = function
  | ConstInt i | ConstChar i | ConstUChar i -> Int32.to_float i
  | ConstUInt ui -> Uint32.to_float ui
  | ConstLong l -> Int64.to_float l
  | ConstULong ul -> Uint64.to_float ul
  | ConstDouble d -> d
;;

(* Converts to c signed char which is 8 bits wide. *)
let convert_to_char = function
  | c ->
    let c = convert_to_int c in
    Int32.of_int8 (Int8.of_int32 c)
;;

(* Converts to c unsigned char which is 8 bits wide. *)
let convert_to_uchar = function
  | c ->
    let c = convert_to_int c in
    Int32.of_uint8 (Uint8.of_int32 c)
;;

let is_zero = function
  | ConstInt 0l -> true
  | ConstUInt ui -> ui = 0i
  | ConstLong 0L -> true
  | ConstULong ul -> ul = 0I
  | ConstDouble 0.0 -> true
  | _ -> false
;;

let evaluate_int32_binary bop l r =
  match bop with
  | Add -> Int32.add l r
  | Sub -> Int32.sub l r
  | Mul -> Int32.mul l r
  | Div -> if r = 0l then 0l else Int32.div l r
  | Rem -> if r = 0l then 0l else Int32.rem l r
  | BAnd -> Int32.logand l r
  | BOr -> Int32.logor l r
  | Xor -> Int32.logxor l r
  | Lsft -> Int32.shift_left l (Int32.to_int r)
  | Rsft -> Int32.shift_right l (Int32.to_int r)
  | And -> if l != 0l && r != 0l then 1l else 0l
  | Or -> if l != 0l || r != 0l then 1l else 0l
  | Equal -> if l = r then 1l else 0l
  | NEqual -> if l <> r then 1l else 0l
  | LEqual -> if l <= r then 1l else 0l
  | GEqual -> if l >= r then 1l else 0l
  | Less -> if l < r then 1l else 0l
  | Greater -> if l > r then 1l else 0l
;;

let evaluate_uint32_binary bop l r =
  match bop with
  | Add -> Uint32.add l r
  | Sub -> Uint32.sub l r
  | Mul -> Uint32.mul l r
  | Div -> if r = 0i then 0i else Uint32.div l r
  | Rem -> if r = 0i then 0i else Uint32.rem l r
  | BAnd -> Uint32.logand l r
  | BOr -> Uint32.logor l r
  | Xor -> Uint32.logxor l r
  | Lsft -> Uint32.shift_left l (Uint32.to_int r)
  | Rsft -> Uint32.shift_right l (Uint32.to_int r)
  | And -> if l != 0i && r != 0i then 1i else 0i
  | Or -> if l != 0i || r != 0i then 1i else 0i
  | Equal -> if l = r then 1i else 0i
  | NEqual -> if l <> r then 1i else 0i
  | LEqual -> if l <= r then 1i else 0i
  | GEqual -> if l >= r then 1i else 0i
  | Less -> if l < r then 1i else 0i
  | Greater -> if l > r then 1i else 0i
;;

let evaluate_int64_binary bop l r =
  match bop with
  | Add -> Int64.add l r
  | Sub -> Int64.sub l r
  | Mul -> if r = 0L then 0L else Int64.mul l r
  | Div -> if r = 0L then 0L else Int64.div l r
  | Rem -> Int64.rem l r
  | BAnd -> Int64.logand l r
  | BOr -> Int64.logor l r
  | Xor -> Int64.logxor l r
  | Lsft -> Int64.shift_left l (Int64.to_int r)
  | Rsft -> Int64.shift_right l (Int64.to_int r)
  | And -> if l != 0L && r != 0L then 1L else 0L
  | Or -> if l != 0L || r != 0L then 1L else 0L
  | Equal -> if l = r then 1L else 0L
  | NEqual -> if l <> r then 1L else 0L
  | LEqual -> if l <= r then 1L else 0L
  | GEqual -> if l >= r then 1L else 0L
  | Less -> if l < r then 1L else 0L
  | Greater -> if l > r then 1L else 0L
;;

let evaluate_uint64_binary bop l r =
  match bop with
  | Add -> Uint64.add l r
  | Sub -> Uint64.sub l r
  | Mul -> Uint64.mul l r
  | Div -> if r = 0I then 0I else Uint64.div l r
  | Rem -> if r = 0I then 0I else Uint64.rem l r
  | BAnd -> Uint64.logand l r
  | BOr -> Uint64.logor l r
  | Xor -> Uint64.logxor l r
  | Lsft -> Uint64.shift_left l (Uint64.to_int r)
  | Rsft -> Uint64.shift_right l (Uint64.to_int r)
  | And -> if l != 0I && r != 0I then 1I else 0I
  | Or -> if l != 0I || r != 0I then 1I else 0I
  | Equal -> if l = r then 1I else 0I
  | NEqual -> if l <> r then 1I else 0I
  | LEqual -> if l <= r then 1I else 0I
  | GEqual -> if l >= r then 1I else 0I
  | Less -> if l < r then 1I else 0I
  | Greater -> if l > r then 1I else 0I
;;

let evaluate_double_binary bop l r =
  match bop with
  | Add -> l +. r
  | Sub -> l -. r
  | Mul -> l *. r
  | Div -> if r = 0.0 then 0.0 else l /. r
  | Rem -> assert false
  | BAnd -> assert false
  | BOr -> assert false
  | Xor -> assert false
  | Lsft -> assert false
  | Rsft -> assert false
  | And -> if l != 0.0 && r != 0.0 then 1.0 else 0.0
  | Or -> if l != 0.0 || r != 0.0 then 1.0 else 0.0
  | Equal -> if l = r then 1.0 else 0.0
  | NEqual -> if l <> r then 1.0 else 0.0
  | LEqual -> if l <= r then 1.0 else 0.0
  | GEqual -> if l >= r then 1.0 else 0.0
  | Less -> if l < r then 1.0 else 0.0
  | Greater -> if l > r then 1.0 else 0.0
;;

let evaluate_binary bop l r =
  assert (const_type l = const_type r);
  let ctp = const_type l in
  match ctp with
  | Char | SChar | UChar -> assert false
  | Int -> ConstInt (evaluate_int32_binary bop (convert_to_int l) (convert_to_int r))
  | UInt -> ConstUInt (evaluate_uint32_binary bop (convert_to_uint l) (convert_to_uint r))
  | Long -> ConstLong (evaluate_int64_binary bop (convert_to_long l) (convert_to_long r))
  | ULong ->
    ConstULong (evaluate_uint64_binary bop (convert_to_ulong l) (convert_to_ulong r))
  | Double ->
    ConstDouble (evaluate_double_binary bop (convert_to_double l) (convert_to_double r))
  | FunType _ -> assert false
  | Pointer _ -> assert false
  | CArray _ -> assert false
;;

let evaluate_int32_unary uop x =
  match uop with
  | Complement -> Int32.lognot x
  | Negate -> Int32.neg x
  | Not -> if x = 0l then 1l else 0l
;;

let evaluate_uint32_unary uop x =
  match uop with
  | Complement -> Uint32.lognot x
  | Negate -> Uint32.neg x
  | Not -> if x = 0i then 1i else 0i
;;

let evaluate_int64_unary uop x =
  match uop with
  | Complement -> Int64.lognot x
  | Negate -> Int64.neg x
  | Not -> if x = 0L then 1L else 0L
;;

let evaluate_uint64_unary uop x =
  match uop with
  | Complement -> Uint64.lognot x
  | Negate -> Uint64.neg x
  | Not -> if x = 0I then 1I else 0I
;;

let evaluate_double_unary uop x =
  match uop with
  | Complement -> assert false
  | Negate -> -.x
  | Not -> if x = 0.0 then 1.0 else 0.0
;;

let evaluate_unary uop x =
  let ctp = const_type x in
  match ctp with
  | Char | SChar | UChar -> assert false
  | Int -> ConstInt (evaluate_int32_unary uop (convert_to_int x))
  | UInt -> ConstUInt (evaluate_uint32_unary uop (convert_to_uint x))
  | Long -> ConstLong (evaluate_int64_unary uop (convert_to_long x))
  | ULong -> ConstULong (evaluate_uint64_unary uop (convert_to_ulong x))
  | Double -> ConstDouble (evaluate_double_unary uop (convert_to_double x))
  | FunType _ -> assert false
  | Pointer _ -> assert false
  | CArray _ -> assert false
;;

let evaluate_conditional cnd lhs rhs =
  assert (const_type lhs = const_type rhs);
  let ctp = const_type cnd in
  let zr = c_type_zero ctp in
  if cnd <> zr then lhs else rhs
;;
