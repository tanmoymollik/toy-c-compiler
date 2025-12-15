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

type value =
  | Constant of Common.const
  | Var of Common.identifier
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
  | Jump of Common.identifier
  | JumpIfZero of value * Common.identifier (* cond * target *)
  | JumpIfNotZero of value * Common.identifier (* cond * target *)
  | Label of Common.identifier
  | FunCall of
      { name : Common.identifier
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
      { name : Common.identifier
      ; global : bool
      ; params : Common.identifier list
      ; body : instruction list
      }
  | StaticVar of
      { name : Common.identifier
      ; global : bool
      ; init : Common.const
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]
