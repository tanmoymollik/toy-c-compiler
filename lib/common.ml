open Stdint

type identifier = Identifier of string [@@deriving show]

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

type asm_type =
  | Byte
  | Word
  | DWord
  | QWord
  | AsmDouble
[@@deriving show]

(* Returns the size of type in bytes. *)
let size = function
  | Int -> 4
  | UInt -> 4
  | Long -> 8
  | ULong -> 8
  | Double -> 8
  | FunType _ -> assert false
;;

(* Returns whether the type is signed. *)
let signed_c_type = function
  | Int | Long | Double -> true
  | UInt | ULong -> false
  | FunType _ -> assert false
;;

let signed_const = function
  | ConstInt _ | ConstLong _ -> true
  | ConstUInt _ | ConstULong _ | ConstDouble _ -> false
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
;;

let c_type_one = function
  | Int -> ConstInt 1l
  | UInt -> ConstUInt 1i
  | Long -> ConstLong 1L
  | ULong -> ConstULong 1I
  | Double -> ConstDouble 1.0
  | FunType _ -> assert false
;;
