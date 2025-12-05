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
  | And
  | Or
  | Xor
  | Lsft
  | Rsft
  | Equal
  | NEqual
  | Less
  | LEqual
  | Greater
  | GEqual
[@@deriving show]

(* Reuse C_ast.const *)
type const =
  | ConstInt of int32
  | ConstUInt of
      (uint32[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint32.to_string v)])
  | ConstLong of int64
  | ConstULong of
      (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
[@@deriving show]

type value =
  | Constant of const
  | Var of identifier
[@@deriving show]

type instruction =
  | Ret of value
  | Unary of
      { uop : unary_op
      ; src : value
      ; dst : value
      }
  | Binary of
      { bop : binary_op
      ; src1 : value
      ; src2 : value
      ; dst : value
      }
  | Copy of
      { src : value
      ; dst : value
      }
  | Jump of identifier
  | JumpIfZero of value * identifier (* cond * target *)
  | JumpIfNotZero of value * identifier (* cond * target *)
  | Label of identifier
  | FunCall of
      { name : identifier
      ; args : value list
      ; dst : value
      }
  | SignExtend of
      { src : value
      ; dst : value
      }
  | Truncate of
      { src : value
      ; dst : value
      }
  | ZeroExtend of
      { src : value
      ; dst : value
      }
[@@deriving show]

type top_level =
  | Function of
      { name : identifier
      ; global : bool
      ; params : identifier list
      ; body : instruction list
      }
  | StaticVar of
      { name : identifier
      ; global : bool
      ; init : Core.static_init
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]
