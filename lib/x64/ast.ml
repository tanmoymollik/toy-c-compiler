open Stdint
open Common

type unary_op =
  | Neg
  | Not
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Imul
  | DivDouble
  | And
  | Or
  | Xor
  | Sal
  | Sar
  | Shr
[@@deriving show]

type cond_code =
  | E
  | NE
  | G
  | GE
  | L
  | LE
  | A
  | AE
  | B
  | BE
[@@deriving show]

type reg =
  | Ax
  | Bx
  | Cx
  | Dx
  | Di
  | Si
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | Sp
  | Bp
  | Xmm0
  | Xmm1
  | Xmm2
  | Xmm3
  | Xmm4
  | Xmm5
  | Xmm6
  | Xmm7
  | Xmm8
  | Xmm9
  | Xmm10
  | Xmm11
  | Xmm12
  | Xmm13
  | Xmm14
  | Xmm15
[@@deriving show]

let int_regs = [ Di; Si; Dx; Cx; R8; R9 ]
let double_regs = [ Xmm0; Xmm1; Xmm2; Xmm3; Xmm4; Xmm5; Xmm6; Xmm7 ]

let is_arg_reg = function
  | Di | Si | Dx | Cx | R8 | R9 -> true
  | _ -> false
;;

type operand =
  | Imm of (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
  | Reg of reg
  | Memory of reg * int
  | Data of identifier
  | Indexed of
      { base : reg
      ; ind : reg
      ; scale : int
      }
  | Pseudo of identifier
  | PseudoMem of identifier * int
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      ; tp : asm_type
      }
  | Movsx of
      { src : operand
      ; dst : operand
      }
  | MovZeroExtend of
      { src : operand
      ; dst : operand
      }
  | Lea of
      { src : operand
      ; dst : operand
      }
  | Cvttsd2si of
      { src : operand
      ; dst : operand
      ; dst_tp : asm_type
      }
  | Cvtsi2sd of
      { src : operand
      ; dst : operand
      ; src_tp : asm_type
      }
  | Unary of unary_op * operand * asm_type
  | Binary of
      { bop : binary_op
      ; src : operand
      ; dst : operand (* dst is always guaranteed to be Stack _. *)
      ; tp : asm_type
      }
  | Cmp of
      { lhs : operand
      ; rhs : operand
      ; tp : asm_type
      }
  | Idiv of operand * asm_type
  | Div of operand * asm_type
  | Cdq of asm_type
  | Jmp of identifier
  | JmpP of identifier
  | JmpC of cond_code * identifier
  | SetC of cond_code * operand
  | Label of identifier
  | Push of operand
  | Pop of reg
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
      ; alignment : int
      ; init_list : static_init list
      }
  | StaticConstant of
      { name : identifier
      ; alignment : int
      ; init : static_init
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]

let is_callee_saved_reg = function
  | Bx | R12 | R13 | R14 | R15 -> true
  | _ -> false
;;

let alloc_stack_ins offset =
  assert (offset > 0);
  Binary { bop = Sub; src = Imm (Uint64.of_int offset); dst = Reg Sp; tp = QWord }
;;

let dealloc_stack_ins offset =
  assert (offset > 0);
  Binary { bop = Add; src = Imm (Uint64.of_int offset); dst = Reg Sp; tp = QWord }
;;
