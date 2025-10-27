type identifier = Identifier of string [@@deriving show]

type operand =
  | Imm of int
  | Register
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand
      }
  | Ret
[@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : instruction list
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
