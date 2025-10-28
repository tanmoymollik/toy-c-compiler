type identifier = Identifier of string [@@deriving show]

type unary_op =
  | Complement
  | Negate
[@@deriving show]

type value =
  | Constant of int
  | Var of identifier
[@@deriving show]

type instruction =
  | Unary of
      { uop : unary_op
      ; src : value
      ; dst : value
      }
  | Ret of value
[@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : instruction list
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
