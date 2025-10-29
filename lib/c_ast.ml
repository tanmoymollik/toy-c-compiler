type identifier = Identifier of string [@@deriving show]

type unary_op =
  | Complement
  | Negate
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
[@@deriving show]

type expression =
  | Constant of int
  | Unary of unary_op * expression
  | Binary of
      { bop : binary_op
      ; lexp : expression
      ; rexp : expression
      }
[@@deriving show]

type statement = Return of expression [@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : statement
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
