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
  | Cx
  | Dx
  | Di
  | Si
  | R8
  | R9
  | R10
  | R11
[@@deriving show]

let arg_regs = [ Di; Si; Dx; Cx; R8; R9 ]
let arg_regs_len = 6

let is_arg_reg = function
  | Di | Si | Dx | Cx | R8 | R9 -> true
  | _ -> false
;;

type operand =
  | Imm of int
  | Reg of reg
  | Stack of int
  | Data of identifier
[@@deriving show]

type operand_size =
  | Byte
  | Word
  | DWord
  | QWord
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      ; sz : operand_size
      }
  | Unary of unary_op * operand * operand_size
  | Binary of
      { bop : binary_op
      ; src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      ; sz : operand_size
      }
  | Cmp of
      { lhs : operand
      ; rhs : operand
      ; sz : operand_size
      }
  | Idiv of operand * operand_size
  | Cdq
  | Jmp of identifier
  | JmpC of cond_code * identifier
  | SetC of cond_code * operand
  | Label of identifier
  | AllocStack of int
  | DeallocStack of int
  | Push of operand
  | Call of identifier
  | Ret
[@@deriving show]

type top_level =
  | Function of
      { name : identifier
      ; global : bool
      ; body : instruction list
      }
  | StaticVar of
      { name : identifier
      ; global : bool
      ; init : Core.static_init
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]
