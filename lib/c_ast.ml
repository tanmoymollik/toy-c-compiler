open Stdint

type identifier = Identifier of string [@@deriving show]

type c_type =
  | Int
  | UInt
  | Long
  | ULong
  | FunType of
      { params : c_type list
      ; ret : c_type
      }
[@@deriving show]

type const =
  | ConstInt of int32
  | ConstUInt of
      (uint32[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint32.to_string v)])
  | ConstLong of int64
  | ConstULong of
      (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
[@@deriving show]

type unary_op =
  | Complement
  | Negate
  | Not
[@@deriving show]

type tunary_op =
  | Inc
  | Dec
[@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | BAnd
  | BOr
  | Xor
  | Lsft
  | Rsft
  | And
  | Or
  | Equal
  | NEqual
  | LEqual
  | GEqual
  | Less
  | Greater
[@@deriving show]

type expression =
  | Constant of const * c_type
  | Var of identifier * c_type
  | Cast of
      { tgt : c_type
      ; exp : expression
      ; etp : c_type
      }
  | Unary of unary_op * expression * c_type
  | TUnary of tunary_op * bool * expression * c_type
  | Binary of
      { bop : binary_op
      ; lexp : expression
      ; rexp : expression
      ; etp : c_type
      }
  | Assignment of
      { lval : expression
      ; rval : expression
      ; etp : c_type
      }
  | Conditional of
      { cnd : expression
      ; lhs : expression
      ; rhs : expression
      ; etp : c_type
      }
  | FunctionCall of identifier * expression list * c_type
[@@deriving show]

type storage_class =
  | Static
  | Extern
[@@deriving show]

type variable_decl =
  { name : identifier
  ; init : expression option
  ; vtp : c_type
  ; storage : storage_class option
  }
[@@deriving show]

type for_init =
  | InitDecl of variable_decl
  | InitExp of expression option
[@@deriving show]

type statement =
  | Return of expression
  | Expression of expression
  | If of
      { cnd : expression
      ; thn : statement
      ; els : statement option
      }
  | Goto of identifier
  | Label of identifier * statement
  | Compound of block
  | Break of identifier
  | Continue of identifier
  | While of expression * statement * identifier
  | DoWhile of statement * expression * identifier
  | For of
      { init : for_init
      ; cnd : expression option
      ; post : expression option
      ; body : statement
      ; label : identifier
      }
  | Switch of
      { cnd : expression
      ; body : statement
      ; cases : const list
      ; default : bool
      ; label : identifier
      }
  | Case of expression * statement * identifier
  | Default of statement * identifier
  | Null
[@@deriving show]

and block_item =
  | S of statement
  | D of declaration
[@@deriving show]

and block = Block of block_item list [@@deriving show]

and function_decl =
  { name : identifier
  ; params : identifier list
  ; body : block option
  ; ftp : c_type
  ; storage : storage_class option
  }
[@@deriving show]

and declaration =
  | FunDecl of function_decl
  | VarDecl of variable_decl
[@@deriving show]

type program = Program of declaration list [@@deriving show]

let get_type = function
  | Constant (_, etp) -> etp
  | Var (_, etp) -> etp
  | Cast { etp; _ } -> etp
  | Unary (_, _, etp) -> etp
  | TUnary (_, _, _, etp) -> etp
  | Binary { etp; _ } -> etp
  | Assignment { etp; _ } -> etp
  | Conditional { etp; _ } -> etp
  | FunctionCall (_, _, etp) -> etp
;;
