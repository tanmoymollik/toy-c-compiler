open Stdint
open Common

type unary_op =
  | Neg
  | Not
[@@deriving show]

type reg =
  | A0
  | T0
  | T1
[@@deriving show]

type operand =
  | Imm of (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
  | Reg of reg
  | Stack of int
[@@deriving show]

type instruction =
  | Mov of
      { src : operand
      ; dst : operand
      }
  | Unary of
      { uop : unary_op
      ; src : operand
      ; dst : operand
      }
  | Li of
      { src : operand (* Imm *)
      ; dst : operand (* Reg *)
      }
  | Ld of
      { src : operand (* Stack *)
      ; dst : operand (* Reg *)
      }
  | Sd of
      { src : operand (* Reg *)
      ; dst : operand (* Stack *)
      }
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
