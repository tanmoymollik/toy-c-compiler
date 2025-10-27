type identifier = Identifier of string [@@deriving show]
type expression = Constant of int [@@deriving show]
type statement = Return of expression [@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : statement
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
