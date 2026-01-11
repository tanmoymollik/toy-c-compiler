open AsmUtils
open Common
open Stdint
open Ast

let gen_stack_for_var fun_iden var_iden =
  let addr = get_stack_address (fun_iden, var_iden) in
  Stack addr
;;

let gen_uop : Common.unary_op -> Ast.unary_op = function
  | Complement -> Not
  | Negate -> Neg
  (* Handled differently. *)
  | Not -> assert false
;;

let gen_const = function
  | ConstInt i -> Imm (Uint64.of_int32 i)
  | ConstUInt ui -> Imm (Uint64.of_uint32 ui)
  | ConstLong l -> Imm (Uint64.of_int64 l)
  | ConstULong ul -> Imm ul
  | ConstDouble _ | ConstChar _ | ConstUChar _ -> assert false
;;

let gen_value fun_name = function
  | Tacky.Ast.Constant c -> gen_const c
  | Tacky.Ast.Var name ->
    (* if SymbolMap.is_static_var name
    then Data name *)
    gen_stack_for_var fun_name name
;;

let gen_instruction fun_name = function
  | Tacky.Ast.Unary { uop; src; dst } ->
    [ Unary
        { uop = gen_uop uop; src = gen_value fun_name src; dst = gen_value fun_name dst }
    ]
  | Tacky.Ast.Ret v -> [ Mov { src = gen_value fun_name v; dst = Reg A0 }; Ret ]
  | _ -> assert false
;;

let gen_top_level = function
  | Tacky.Ast.Function { name; body; _ } ->
    let ins = List.concat_map (gen_instruction name) body in
    let alloc_stack = get_fun_stack_alloc name in
    let ins = if alloc_stack <> 0 then AllocStack alloc_stack :: ins else ins in
    let body = List.concat_map InsFixer.fix_instruction ins in
    Function { name; body }
  | Tacky.Ast.StaticVar _ -> assert false
  | Tacky.Ast.StaticConstant _ -> assert false
;;

let gen_program = function
  | Tacky.Ast.Program tpns -> Program (List.map gen_top_level tpns)
;;
