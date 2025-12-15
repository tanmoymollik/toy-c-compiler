open Stdint
open Common
open AsmUtils

let gen_uop = function
  | Tacky.Complement -> Ast.Not
  | Tacky.Negate -> Ast.Neg
  (* Handled differently. *)
  | _ -> assert false
;;

let gen_bop = function
  | Tacky.Add -> Ast.Add
  | Tacky.Sub -> Ast.Sub
  | Tacky.Mul -> Ast.Imul
  | Tacky.And -> Ast.And
  | Tacky.Or -> Ast.Or
  | Tacky.Xor -> Ast.Xor
  | Tacky.Lsft -> Ast.Sal
  | Tacky.Rsft -> Ast.Sar
  (* Handled differently. *)
  | Tacky.Div
  | Tacky.Rem
  | Tacky.Equal
  | Tacky.NEqual
  | Tacky.Less
  | Tacky.LEqual
  | Tacky.Greater
  | Tacky.GEqual -> assert false
;;

let gen_cond_code signed = function
  | Tacky.Equal -> Ast.E
  | Tacky.NEqual -> Ast.NE
  | Tacky.Less -> if signed then Ast.L else Ast.B
  | Tacky.LEqual -> if signed then Ast.LE else Ast.BE
  | Tacky.Greater -> if signed then Ast.G else Ast.A
  | Tacky.GEqual -> if signed then Ast.GE else Ast.AE
  (* Not conditional ops. *)
  | _ -> assert false
;;

let alloc_stack_ins offset =
  assert (offset > 0);
  Ast.Binary
    { bop = Ast.Sub
    ; src = Ast.Imm (Uint64.of_int offset)
    ; dst = Ast.Reg Ast.Sp
    ; sz = QWord
    }
;;

let dealloc_stack_ins offset =
  assert (offset > 0);
  Ast.Binary
    { bop = Ast.Add
    ; src = Ast.Imm (Uint64.of_int offset)
    ; dst = Ast.Reg Ast.Sp
    ; sz = QWord
    }
;;

let gen_stack_for_var fun_iden var_iden =
  let addr = get_stack_address (fun_iden, var_iden) in
  Ast.Stack addr
;;

let gen_const = function
  | ConstInt i -> Ast.Imm (Uint64.of_int32 i)
  | ConstUInt ui -> Ast.Imm (Uint64.of_uint32 ui)
  | ConstLong l -> Ast.Imm (Uint64.of_int64 l)
  | ConstULong ul -> Ast.Imm ul
;;

let gen_value fun_name = function
  | Tacky.Constant c -> gen_const c
  | Tacky.Var name ->
    if Symbol_map.is_static_var name
    then Ast.Data name
    else gen_stack_for_var fun_name name
;;

let signed = function
  | Tacky.Constant _ -> assert false
  | Tacky.Var name -> Symbol_map.is_signed_var name
;;

let gen_instruction fun_name = function
  | Tacky.Ret v ->
    let sz = get_oprnd_size_for_val v in
    [ Ast.Mov { src = gen_value fun_name v; dst = Ast.Reg Ast.Ax; sz }; Ast.Ret ]
  (* dst is always Tacky.Var *)
  | Tacky.Unary { uop; src; dst } ->
    (match uop with
     | Tacky.Complement | Tacky.Negate ->
       let sz = get_oprnd_size_for_val dst in
       [ Ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst; sz }
       ; Ast.Unary (gen_uop uop, gen_value fun_name dst, sz)
       ]
     | Tacky.Not ->
       let src_sz = get_oprnd_size_for_val src in
       let dst_sz = get_oprnd_size_for_val dst in
       let dst = gen_value fun_name dst in
       let zr = Ast.Imm 0I in
       [ Ast.Cmp { lhs = gen_value fun_name src; rhs = zr; sz = src_sz }
       ; Ast.Mov { src = zr; dst; sz = dst_sz }
       ; Ast.SetC (E, dst)
       ])
  (* dst is always Tacky.Var *)
  | Tacky.Binary { bop; src1; src2; dst } ->
    let op_sz = get_oprnd_size_for_val dst in
    let signed = signed dst in
    (match bop with
     | Tacky.Div | Tacky.Rem ->
       let ex_ins =
         if signed
         then Ast.Cdq op_sz
         else Ast.Mov { src = Ast.Imm 0I; dst = Ast.Reg Ast.Dx; sz = op_sz }
       in
       let tmp_dst =
         match bop with
         | Tacky.Div -> Ast.Reg Ast.Ax
         | Tacky.Rem -> Ast.Reg Ast.Dx
         | _ -> assert false
       in
       let div_ins =
         if signed
         then Ast.Idiv (gen_value fun_name src2, op_sz)
         else Ast.Div (gen_value fun_name src2, op_sz)
       in
       [ Ast.Mov { src = gen_value fun_name src1; dst = Ast.Reg Ast.Ax; sz = op_sz }
       ; ex_ins
       ; div_ins
       ; Ast.Mov { src = tmp_dst; dst = gen_value fun_name dst; sz = op_sz }
       ]
     | Tacky.Equal
     | Tacky.NEqual
     | Tacky.Less
     | Tacky.LEqual
     | Tacky.Greater
     | Tacky.GEqual ->
       let dst = gen_value fun_name dst in
       let zr = Ast.Imm 0I in
       [ Ast.Cmp
           { lhs = gen_value fun_name src1; rhs = gen_value fun_name src2; sz = op_sz }
       ; Ast.Mov { src = zr; dst; sz = op_sz }
       ; Ast.SetC (gen_cond_code signed bop, dst)
       ]
     | Tacky.Add | Tacky.Sub | Tacky.Mul | Tacky.And | Tacky.Or | Tacky.Xor ->
       let dst = gen_value fun_name dst in
       [ Ast.Mov { src = gen_value fun_name src1; dst; sz = op_sz }
       ; Ast.Binary { bop = gen_bop bop; src = gen_value fun_name src2; dst; sz = op_sz }
       ]
     | Tacky.Lsft | Tacky.Rsft ->
       let src2 = gen_value fun_name src2 in
       let tmp_src2 = Ast.Reg Ast.Cx in
       let dst = gen_value fun_name dst in
       let bop = gen_bop bop in
       let bop = if (not signed) && bop = Ast.Sar then Ast.Shr else bop in
       [ Ast.Mov { src = gen_value fun_name src1; dst; sz = op_sz }
       ; Ast.Mov { src = src2; dst = tmp_src2; sz = op_sz }
       ; Ast.Binary { bop; src = tmp_src2; dst; sz = op_sz }
       ])
  | Tacky.Copy { src; dst } ->
    let sz = get_oprnd_size_for_val dst in
    [ Ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst; sz } ]
  | Tacky.Jump iden -> [ Ast.Jmp iden ]
  | Tacky.JumpIfZero (cnd, tgt) ->
    let sz = get_oprnd_size_for_val cnd in
    let zr = Ast.Imm 0I in
    [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; sz }; Ast.JmpC (Ast.E, tgt) ]
  | Tacky.JumpIfNotZero (cnd, tgt) ->
    let sz = get_oprnd_size_for_val cnd in
    let zr = Ast.Imm 0I in
    [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; sz }; Ast.JmpC (Ast.NE, tgt) ]
  | Tacky.Label iden -> [ Ast.Label iden ]
  | Tacky.FunCall { name; args; dst } ->
    let arg_regs = Ast.arg_regs in
    let arg_regs_len = Ast.arg_regs_len in
    let reg_args, stk_args = List.take arg_regs_len args, List.drop arg_regs_len args in
    let stack_padding = if List.length stk_args mod 2 = 1 then 8 else 0 in
    let stack_alloc = if stack_padding <> 0 then [ alloc_stack_ins 8 ] else [] in
    let f ind arg =
      let src = gen_value fun_name arg in
      let dst = Ast.Reg (List.nth arg_regs ind) in
      let sz = get_oprnd_size_for_val arg in
      Ast.Mov { src; dst; sz }
    in
    let reg_arg_ins = List.mapi f reg_args in
    let f arg =
      let src = gen_value fun_name arg in
      let sz = get_oprnd_size_for_val arg in
      match sz with
      | QWord -> [ Ast.Push src ]
      | _ ->
        (match src with
         | Ast.Imm _ | Ast.Reg _ -> [ Ast.Push src ]
         | _ ->
           let dst = Ast.Reg Ast.Ax in
           [ Ast.Mov { src; dst; sz }; Ast.Push dst ])
    in
    let stk_arg_ins = List.concat_map f (List.rev stk_args) in
    let fun_call = [ Ast.Call name ] in
    let bytes_to_remove = (8 * List.length stk_args) + stack_padding in
    let stack_dealloc =
      if bytes_to_remove <> 0 then [ dealloc_stack_ins bytes_to_remove ] else []
    in
    let dst = gen_value fun_name dst in
    let src = Ast.Reg Ast.Ax in
    let sz = get_oprnd_size_for_fun_ret name in
    let ret_ins = [ Ast.Mov { src; dst; sz } ] in
    stack_alloc @ reg_arg_ins @ stk_arg_ins @ fun_call @ stack_dealloc @ ret_ins
  | Tacky.SignExtend { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Movsx { src; dst } ]
  | Tacky.Truncate { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Mov { src; dst; sz = DWord } ]
  | Tacky.ZeroExtend { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.MovZeroExtend { src; dst } ]
;;

let gen_top_level = function
  | Tacky.Function { name; global; params; body } ->
    let arg_regs = Ast.arg_regs in
    let arg_regs_len = Ast.arg_regs_len in
    let reg_params, stk_params =
      List.take arg_regs_len params, List.drop arg_regs_len params
    in
    let f ind arg =
      let dst = gen_stack_for_var name arg in
      let src = Ast.Reg (List.nth arg_regs ind) in
      let sz = get_oprnd_size_for_iden arg in
      Ast.Mov { src; dst; sz }
    in
    let reg_param_ins = List.mapi f reg_params in
    let f ind arg =
      let dst = gen_stack_for_var name arg in
      let src = Ast.Stack (-(16 + (8 * ind))) in
      let sz = get_oprnd_size_for_iden arg in
      Ast.Mov { src; dst; sz }
    in
    let stk_param_ins = List.mapi f stk_params in
    let body =
      reg_param_ins @ stk_param_ins @ List.concat_map (gen_instruction name) body
    in
    let alloc_stack = get_fun_stack_alloc name in
    let align16 = if alloc_stack > 0 then 16 * ((alloc_stack / 16) + 1) else 0 in
    let body = if align16 > 0 then alloc_stack_ins align16 :: body else body in
    let body = List.concat_map Ins_fixer.fix_instruction body in
    Ast.Function { name; global; body }
  | Tacky.StaticVar { name; global; init } -> Ast.StaticVar { name; global; init }
;;

let gen_program = function
  | Tacky.Program tpns -> Ast.Program (List.map gen_top_level tpns)
;;
