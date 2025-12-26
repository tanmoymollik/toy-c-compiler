open Stdint

type identifier = Identifier of string [@@deriving show]

type unary_op =
  | Complement
  | Negate
  | Not
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | BAnd
  | BOr
  | Xor
  | Lsft
  | Rsft
  | And
  | Or
  | Equal
  | NEqual
  | Less
  | LEqual
  | Greater
  | GEqual
[@@deriving show]

type c_type =
  | Int
  | UInt
  | Long
  | ULong
  | Double
  | FunType of
      { params : c_type list
      ; ret : c_type
      }
  | Pointer of c_type
  | CArray of c_type * int
[@@deriving show]

type const =
  | ConstInt of int32
  | ConstUInt of
      (uint32[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint32.to_string v)])
  | ConstLong of int64
  | ConstULong of
      (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
  | ConstDouble of float
[@@deriving show]

type static_init =
  | IntInit of int32
  | UIntInit of
      (uint32[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint32.to_string v)])
  | LongInit of int64
  | ULongInit of
      (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
  | DoubleInit of float
  | ZeroInit of { bytes : int }
[@@deriving show]

type asm_type =
  | Byte
  | Word
  | DWord
  | QWord
  | AsmDouble
  | ByteArray of
      { sz : int
      ; alignment : int
      }
[@@deriving show]

(* Returns the size of type in bytes. *)
let rec size = function
  | Int -> 4
  | UInt -> 4
  | Long -> 8
  | ULong -> 8
  | Double -> 8
  | FunType _ -> assert false
  | Pointer _ -> 8
  | CArray (tp, sz) -> sz * size tp
;;

(* Returns whether the type is signed. *)
let signed_c_type = function
  | Int | Long | Double -> true
  | UInt | ULong | Pointer _ | CArray _ -> false
  | FunType _ -> assert false
;;

let signed_const = function
  | ConstInt _ | ConstLong _ -> true
  | ConstUInt _ | ConstULong _ | ConstDouble _ -> false
;;

let is_arithmetic_type = function
  | Int | UInt | Long | ULong | Double -> true
  | FunType _ | Pointer _ | CArray _ -> false
;;

let is_integer_type = function
  | Int | UInt | Long | ULong -> true
  | Double | FunType _ | Pointer _ | CArray _ -> false
;;

let is_pointer_type = function
  | Pointer _ -> true
  | _ -> false
;;

let is_array_type = function
  | CArray _ -> true
  | _ -> false
;;

(* Returns the type both t1 and t2 should be converted to. *)
let get_common_type t1 t2 =
  if t1 = t2
  then t1
  else if t1 = Double || t2 = Double
  then Double
  else if size t1 = size t2
  then if signed_c_type t1 then t2 else t1
  else if size t1 > size t2
  then t1
  else t2
;;

let c_type_zero = function
  | Int -> ConstInt 0l
  | UInt -> ConstUInt 0i
  | Long -> ConstLong 0L
  | ULong -> ConstULong 0I
  | Double -> ConstDouble 0.0
  | FunType _ -> assert false
  | Pointer _ -> ConstULong 0I
  | CArray _ -> assert false
;;

let c_type_one = function
  | Int -> ConstInt 1l
  | UInt -> ConstUInt 1i
  | Long -> ConstLong 1L
  | ULong -> ConstULong 1I
  | Double -> ConstDouble 1.0
  | FunType _ -> assert false
  | Pointer _ -> ConstLong 1L
  | CArray _ -> assert false
;;

let rec scalar_type_alignment = function
  | Int | UInt -> 4
  | Long | ULong | Double | Pointer _ -> 8
  | FunType _ -> assert false
  | CArray (tp, _) -> scalar_type_alignment tp
;;

let get_asm_type_for_c_type = function
  | Int | UInt -> DWord
  | Long | ULong | Pointer _ -> QWord
  | Double -> AsmDouble
  | FunType _ -> assert false
  | CArray (tp, sz) ->
    let sz = sz * size tp in
    let alignment = if sz >= 16 then 16 else scalar_type_alignment tp in
    ByteArray { sz; alignment }
;;

let init_zero tp = ZeroInit { bytes = size tp }
