open Common

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
  | Constant of const
  | Var of identifier
[@@deriving show]

type exp_result =
  | PlainOperand of value
  | DereferencedPointer of value
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
  | SignExtend of
      { src : value
      ; dst : value
      }
  | Truncate of
      { src : value
      ; dst : value
      }
  | ZeroExtend of
      { src : value
      ; dst : value
      }
  | IntToDouble of
      { src : value
      ; dst : value
      }
  | DoubleToInt of
      { src : value
      ; dst : value
      }
  | UIntToDouble of
      { src : value
      ; dst : value
      }
  | DoubleToUInt of
      { src : value
      ; dst : value
      }
  | GetAddr of
      { src : value
      ; dst : value
      }
  | Load of
      { src_ptr : value
      ; dst : value
      }
  | Store of
      { src : value
      ; dst_ptr : value
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
      ; tp : c_type
      ; init : const
      }
[@@deriving show]

type program = Program of top_level list [@@deriving show]
