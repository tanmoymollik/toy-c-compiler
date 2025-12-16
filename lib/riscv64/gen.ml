open AsmUtils
open Common
open Stdint

let gen_stack_for_var fun_iden var_iden =
  let addr = get_stack_address (fun_iden, var_iden) in
  Ast.Stack addr
;;

let gen_uop = function
  | Tacky.Complement -> Ast.Not
  | Tacky.Negate -> Ast.Neg
  (* Handled differently. *)
  | _ -> assert false
;;

let gen_const = function
  | ConstInt i -> Ast.Imm (Uint64.of_int32 i)
  | ConstUInt ui -> Ast.Imm (Uint64.of_uint32 ui)
  | ConstLong l -> Ast.Imm (Uint64.of_int64 l)
  | ConstULong ul -> Ast.Imm ul
  | ConstDouble _ -> assert false
;;

let gen_value fun_name = function
  | Tacky.Constant c -> gen_const c
  | Tacky.Var name ->
    (* if Symbol_map.is_static_var name
    then Ast.Data name *)
    gen_stack_for_var fun_name name
;;

let gen_instruction fun_name = function
  | Tacky.Unary { uop; src; dst } ->
    [ Ast.Unary
        { uop = gen_uop uop; src = gen_value fun_name src; dst = gen_value fun_name dst }
    ]
  | Tacky.Ret v ->
    [ Ast.Mov { src = gen_value fun_name v; dst = Ast.Reg Ast.A0 }; Ast.Ret ]
  | _ -> assert false
;;

let gen_top_level = function
  | Tacky.Function { name; body; _ } ->
    let ins = List.concat_map (gen_instruction name) body in
    let alloc_stack = get_fun_stack_alloc name in
    let ins = if alloc_stack <> 0 then Ast.AllocStack alloc_stack :: ins else ins in
    let body = List.concat_map InsFixer.fix_instruction ins in
    Ast.Function { name; body }
  | Tacky.StaticVar _ -> assert false
;;

let gen_program = function
  | Tacky.Program tpns -> Ast.Program (List.map gen_top_level tpns)
;;
