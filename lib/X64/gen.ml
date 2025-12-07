open Stdint

let get_inner_iden = function
  | Tacky.Identifier iden -> iden
;;

let get_oprnd_size_for_c_type = function
  | C_ast.Int | C_ast.UInt -> Ast.DWord
  | C_ast.Long | C_ast.ULong -> Ast.QWord
  | C_ast.FunType _ -> assert false
;;

let get_oprnd_size_for_iden iden =
  match Hashtbl.find_opt Core.symbol_map iden with
  | Some Core.{ tp; _ } -> get_oprnd_size_for_c_type tp
  | None -> assert false
;;

let get_oprnd_size_for_fun_ret = function
  | Tacky.Identifier iden ->
    (match Hashtbl.find_opt Core.symbol_map iden with
     | Some Core.{ tp = C_ast.FunType { ret; _ }; _ } -> get_oprnd_size_for_c_type ret
     | _ -> assert false)
;;

let get_oprnd_size_for_val = function
  | Tacky.Constant (Tacky.ConstInt _ | Tacky.ConstUInt _) -> Ast.DWord
  | Tacky.Constant (Tacky.ConstLong _ | Tacky.ConstULong _) -> Ast.QWord
  | Tacky.Var (Tacky.Identifier iden) -> get_oprnd_size_for_iden iden
;;

let var_map : (string, int) Hashtbl.t = Hashtbl.create 100
let fun_map : (string, int) Hashtbl.t = Hashtbl.create 100

let get_fun_stack_alloc fun_name =
  match Hashtbl.find_opt fun_map fun_name with
  | Some v -> v
  | None -> 0
;;

let get_stack_address fun_name iden =
  match Hashtbl.find_opt var_map iden with
  | Some v -> v
  | None ->
    let b_sz =
      match get_oprnd_size_for_iden iden with
      | Ast.DWord -> 4
      | Ast.QWord -> 8
      | _ -> assert false
    in
    let stk_ptr = get_fun_stack_alloc fun_name + b_sz in
    Hashtbl.add fun_map fun_name stk_ptr;
    Hashtbl.add var_map iden stk_ptr;
    stk_ptr
;;

let gen_identifier = function
  | Tacky.Identifier ident -> Ast.Identifier ident
;;

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
    ; sz = Ast.QWord
    }
;;

let dealloc_stack_ins offset =
  assert (offset > 0);
  Ast.Binary
    { bop = Ast.Add
    ; src = Ast.Imm (Uint64.of_int offset)
    ; dst = Ast.Reg Ast.Sp
    ; sz = Ast.QWord
    }
;;

let gen_stack_for_var fun_name = function
  | Tacky.Identifier iden ->
    let addr = get_stack_address fun_name iden in
    Ast.Stack addr
;;

let gen_const = function
  | Tacky.ConstInt i -> Ast.Imm (Uint64.of_int32 i)
  | Tacky.ConstUInt ui -> Ast.Imm (Uint64.of_uint32 ui)
  | Tacky.ConstLong l -> Ast.Imm (Uint64.of_int64 l)
  | Tacky.ConstULong ul -> Ast.Imm ul
;;

let gen_value fun_name = function
  | Tacky.Constant c -> gen_const c
  | Tacky.Var (Tacky.Identifier iden) ->
    (match Hashtbl.find_opt Core.symbol_map iden with
     | Some Core.{ attrs = Core.StaticAttr _; _ } -> Ast.Data (Ast.Identifier iden)
     | _ -> gen_stack_for_var fun_name (Tacky.Identifier iden))
;;

let signed = function
  | Tacky.Constant _ -> assert false
  | Tacky.Var (Tacky.Identifier iden) ->
    (match Hashtbl.find_opt Core.symbol_map iden with
     | Some Core.{ tp; _ } -> C_ast.signed tp
     | None -> assert false)
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
  | Tacky.Jump iden -> [ Ast.Jmp (gen_identifier iden) ]
  | Tacky.JumpIfZero (cnd, tgt) ->
    let sz = get_oprnd_size_for_val cnd in
    let zr = Ast.Imm 0I in
    [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; sz }
    ; Ast.JmpC (Ast.E, gen_identifier tgt)
    ]
  | Tacky.JumpIfNotZero (cnd, tgt) ->
    let sz = get_oprnd_size_for_val cnd in
    let zr = Ast.Imm 0I in
    [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; sz }
    ; Ast.JmpC (Ast.NE, gen_identifier tgt)
    ]
  | Tacky.Label iden -> [ Ast.Label (gen_identifier iden) ]
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
      | Ast.QWord -> [ Ast.Push src ]
      | _ ->
        (match src with
         | Ast.Imm _ | Ast.Reg _ -> [ Ast.Push src ]
         | _ ->
           let dst = Ast.Reg Ast.Ax in
           [ Ast.Mov { src; dst; sz }; Ast.Push dst ])
    in
    let stk_arg_ins = List.concat_map f (List.rev stk_args) in
    let fun_call = [ Ast.Call (gen_identifier name) ] in
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
    [ Ast.Mov { src; dst; sz = Ast.DWord } ]
  | Tacky.ZeroExtend { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.MovZeroExtend { src; dst } ]
;;

let gen_top_level = function
  | Tacky.Function { name = Tacky.Identifier name; global; params; body } ->
    let arg_regs = Ast.arg_regs in
    let arg_regs_len = Ast.arg_regs_len in
    let reg_params, stk_params =
      List.take arg_regs_len params, List.drop arg_regs_len params
    in
    let f ind arg =
      let dst = gen_stack_for_var name arg in
      let src = Ast.Reg (List.nth arg_regs ind) in
      let sz = get_oprnd_size_for_iden (get_inner_iden arg) in
      Ast.Mov { src; dst; sz }
    in
    let reg_param_ins = List.mapi f reg_params in
    let f ind arg =
      let dst = gen_stack_for_var name arg in
      let src = Ast.Stack (-(16 + (8 * ind))) in
      let sz = get_oprnd_size_for_iden (get_inner_iden arg) in
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
    Ast.Function { name = Ast.Identifier name; global; body }
  | Tacky.StaticVar { name; global; init } ->
    let name = gen_identifier name in
    Ast.StaticVar { name; global; init }
;;

let gen_program = function
  | Tacky.Program tpns -> Ast.Program (List.map gen_top_level tpns)
;;
