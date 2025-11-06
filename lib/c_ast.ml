type identifier = Identifier of string [@@deriving show]

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

type assign_op =
  | Eq
  | AEq
  | SEq
  | MEq
  | DEq
  | REq
  | BAEq
  | BOEq
  | XEq
  | LsftEq
  | RsftEq
[@@deriving show]

type expression =
  | Constant of int
  | Var of identifier
  | Unary of unary_op * expression
  | TUnary of tunary_op * bool * expression
  | Binary of
      { bop : binary_op
      ; lexp : expression
      ; rexp : expression
      }
  | Assignment of
      { aop : assign_op
      ; lval : expression
      ; rval : expression
      }
  | Conditional of
      { cnd : expression
      ; lhs : expression
      ; rhs : expression
      }
[@@deriving show]

type declaration =
  | Declaration of
      { name : identifier
      ; init : expression option
      }
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
  | Null
[@@deriving show]

and block_item =
  | S of statement
  | D of declaration
[@@deriving show]

and block = Block of block_item list [@@deriving show]

type function_def =
  | Function of
      { name : identifier
      ; body : block
      }
[@@deriving show]

type program = Program of function_def [@@deriving show]
