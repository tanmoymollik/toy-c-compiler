open Common

type tunary_op =
  | Inc
  | Dec
[@@deriving show]

type expression =
  | Constant of const * c_type
  | CString of string * c_type
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
  | CompoundAssign of
      { bop : binary_op
      ; lexp : expression
      ; rexp : expression
      ; btp : c_type
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
  | Dereference of expression * c_type
  | AddrOf of expression * c_type
  | Subscript of expression * expression * c_type
  | SizeOf of expression * c_type
  | SizeOfT of c_type * c_type
  | Dot of
      { struct_exp : expression
      ; member : identifier
      ; etp : c_type
      }
  | Arrow of
      { struct_ptr : expression
      ; member : identifier
      ; etp : c_type
      }
[@@deriving show]

type c_initializer =
  | SingleInit of expression * c_type
  | CompoundInit of c_initializer list * c_type
[@@deriving show]

type storage_class =
  | Static
  | Extern
[@@deriving show]

type variable_decl =
  { name : identifier
  ; init : c_initializer option
  ; vtp : c_type
  ; storage : storage_class option
  }
[@@deriving show]

type member_decl =
  { name : identifier
  ; mtp : c_type
  }
[@@deriving show]

type struct_decl =
  { tag : identifier
  ; members : member_decl list
  }
[@@deriving show]

type for_init =
  | InitDecl of variable_decl
  | InitExp of expression option
[@@deriving show]

type statement =
  | Return of expression option
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
  | StructDecl of struct_decl
[@@deriving show]

type program = Program of declaration list [@@deriving show]

(* Returns the type of the expression. *)
let get_type = function
  | Constant (_, etp)
  | CString (_, etp)
  | Var (_, etp)
  | Cast { etp; _ }
  | Unary (_, _, etp)
  | TUnary (_, _, _, etp)
  | Binary { etp; _ }
  | CompoundAssign { etp; _ }
  | Assignment { etp; _ }
  | Conditional { etp; _ }
  | FunctionCall (_, _, etp)
  | Dereference (_, etp)
  | AddrOf (_, etp)
  | Subscript (_, _, etp)
  | SizeOf (_, etp)
  | SizeOfT (_, etp)
  | Dot { etp; _ }
  | Arrow { etp; _ } -> etp
;;

let is_lvalue = function
  | Var _ -> true
  | Dereference _ -> true
  | Subscript _ -> true
  | CString _ -> true
  | _ -> false
;;

let is_null_pointer_const = function
  | Constant (c, _) ->
    (match c with
     | ConstInt 0l | ConstLong 0L -> true
     | ConstUInt ui -> ui = 0i
     | ConstULong ul -> ul = 0I
     | _ -> false)
  | _ -> false
;;

(* Converts the given expression to the given type. *)
let convert_to t exp = if get_type exp = t then exp else Cast { tgt = t; exp; etp = t }

(* At least one of the expressions must be a pointer type. *)
let get_common_pointer_type e1 e2 =
  let t1 = get_type e1 in
  let t2 = get_type e2 in
  if t1 = t2
  then t1
  else if is_null_pointer_const e1
  then t2
  else if is_null_pointer_const e2
  then t1
  else if t1 = Pointer Void && is_pointer_type t2
  then t1
  else if t2 = Pointer Void && is_pointer_type t1
  then t2
  else raise (Errors.SemanticError "Incompatible pointer types")
;;

let convert_by_assignment tgt exp =
  if get_type exp = tgt
  then exp
  else if is_arithmetic_type (get_type exp) && is_arithmetic_type tgt
  then convert_to tgt exp
  else if is_null_pointer_const exp && is_pointer_type tgt
  then convert_to tgt exp
  else if tgt = Pointer Void && get_type exp |> is_pointer_type
  then convert_to tgt exp
  else if is_pointer_type tgt && get_type exp = Pointer Void
  then convert_to tgt exp
  else raise (Errors.SemanticError "Cannot convert type for assignment")
;;

let rec zero_initializer tp =
  match tp with
  | CArray (e_tp, sz) -> CompoundInit (List.init sz (fun _ -> zero_initializer e_tp), tp)
  | Pointer _ -> SingleInit (Constant (c_type_zero tp, ULong), tp)
  | _ -> SingleInit (Constant (c_type_zero tp, tp), tp)
;;
