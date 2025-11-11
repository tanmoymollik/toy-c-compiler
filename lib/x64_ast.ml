type identifier = Identifier of string [@@deriving show]

type unary_op =
  | Neg
  | Not
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Imul
  | And
  | Or
  | Xor
  | Sal
  | Sar
[@@deriving show]

type cond_code =
  | E
  | NE
  | G
  | GE
  | L
  | LE
[@@deriving show]

type reg =
  | Ax
  | Dx
  | Cx
  | Cl
  | R10
  | R11
[@@deriving show]

type operand =
  | Imm of int
  | Reg of reg
  | Stack of int
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      }
  | Unary of unary_op * operand
  | Binary of
      { bop : binary_op
      ; src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      }
  | Cmp of
      { lhs : operand
      ; rhs : operand
      }
  | Idiv of operand
  | Cdq
  | Jmp of identifier
  | JmpC of cond_code * identifier
  | SetC of cond_code * operand
  | Label of identifier
  | AllocStack of int
  | Ret
[@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : instruction list
      }
[@@deriving show]

type program = Program of function_def list [@@deriving show]
