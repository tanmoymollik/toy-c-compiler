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

type value =
  | Constant of int
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
      ; init : int
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]
