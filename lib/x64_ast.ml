type identifier = Identifier of string [@@deriving show]

type unary_op =
  | Neg
  | Not
[@@deriving show]

type reg =
  | AX
  | R10
[@@deriving show]

type operand =
  | Imm of int
  | Reg of reg
  | Stack of int
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand
      }
  | Unary of unary_op * operand
  | AllocStack of int
  | Ret
[@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : instruction list
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
