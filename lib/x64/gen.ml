open Common
open AsmUtils

let static_consts : (int64 * int, string) Hashtbl.t = Hashtbl.create 10

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
    ; src = Ast.Imm (Stdint.Uint64.of_int offset)
    ; dst = Ast.Reg Ast.Sp
    ; tp = QWord
    }
;;

let dealloc_stack_ins offset =
  assert (offset > 0);
  Ast.Binary
    { bop = Ast.Add
    ; src = Ast.Imm (Stdint.Uint64.of_int offset)
    ; dst = Ast.Reg Ast.Sp
    ; tp = QWord
    }
;;

let gen_stack_for_var fun_iden var_iden =
  let addr = get_stack_address (fun_iden, var_iden) in
  Ast.Stack addr
;;

let get_double_const cd alignment =
  let ci = Int64.bits_of_float cd in
  let const_label =
    match Hashtbl.find_opt static_consts (ci, alignment) with
    | Some lbl -> lbl
    | None ->
      let lbl = Core.make_unique_label Core.static_const_label in
      Hashtbl.replace static_consts (ci, alignment) lbl;
      lbl
  in
  let var = Identifier const_label in
  Ast.Data var
;;

let gen_const = function
  | ConstInt i -> Ast.Imm (Stdint.Uint64.of_int32 i)
  | ConstUInt ui -> Ast.Imm (Stdint.Uint64.of_uint32 ui)
  | ConstLong l -> Ast.Imm (Stdint.Uint64.of_int64 l)
  | ConstULong ul -> Ast.Imm ul
  | ConstDouble d -> get_double_const d 8
;;

let gen_value fun_name = function
  | Tacky.Constant c -> gen_const c
  | Tacky.Var name ->
    if AsmSymbolMap.is_static_var name
    then Ast.Data name
    else gen_stack_for_var fun_name name
;;

let signed = function
  | Tacky.Constant c -> signed_const c
  | Tacky.Var name -> AsmSymbolMap.is_signed_var name
;;

let gen_ins_div fun_name = function
  | Tacky.Binary { bop; src1; src2; dst } ->
    let op_tp = get_asm_type_for_val dst in
    let signed = signed dst in
    (match op_tp, bop with
     | AsmDouble, Tacky.Div ->
       let dst = gen_value fun_name dst in
       [ Ast.Mov { src = gen_value fun_name src1; dst; tp = op_tp }
       ; Ast.Binary
           { bop = Ast.DivDouble; src = gen_value fun_name src2; dst; tp = op_tp }
       ]
     | AsmDouble, Tacky.Rem -> assert false
     | _ ->
       let ex_ins =
         if signed
         then Ast.Cdq op_tp
         else Ast.Mov { src = Ast.Imm 0I; dst = Ast.Reg Ast.Dx; tp = op_tp }
       in
       let tmp_dst =
         match bop with
         | Tacky.Div -> Ast.Reg Ast.Ax
         | Tacky.Rem -> Ast.Reg Ast.Dx
         | _ -> assert false
       in
       let div_ins =
         if signed
         then Ast.Idiv (gen_value fun_name src2, op_tp)
         else Ast.Div (gen_value fun_name src2, op_tp)
       in
       [ Ast.Mov { src = gen_value fun_name src1; dst = Ast.Reg Ast.Ax; tp = op_tp }
       ; ex_ins
       ; div_ins
       ; Ast.Mov { src = tmp_dst; dst = gen_value fun_name dst; tp = op_tp }
       ])
  | _ -> assert false
;;

let gen_ins_uop fun_name = function
  (* dst is always Tacky.Var *)
  | Tacky.Unary { uop; src; dst } ->
    (match uop with
     | Tacky.Complement ->
       let tp = get_asm_type_for_val dst in
       [ Ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst; tp }
       ; Ast.Unary (gen_uop uop, gen_value fun_name dst, tp)
       ]
     | Tacky.Negate ->
       let tp = get_asm_type_for_val dst in
       (match tp with
        | AsmDouble ->
          let neg_zr = get_double_const (-0.0) 16 in
          let dst = gen_value fun_name dst in
          [ Ast.Mov { src = gen_value fun_name src; dst; tp }
          ; Ast.Binary { bop = Ast.Xor; src = neg_zr; dst; tp }
          ]
        | _ ->
          [ Ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst; tp }
          ; Ast.Unary (gen_uop uop, gen_value fun_name dst, tp)
          ])
     | Tacky.Not ->
       let src_tp = get_asm_type_for_val src in
       let dst_tp = get_asm_type_for_val dst in
       let dst = gen_value fun_name dst in
       let zr = Ast.Imm 0I in
       (match src_tp with
        | AsmDouble ->
          let zr_reg = Ast.Reg Ast.Xmm0 in
          [ Ast.Binary { bop = Ast.Xor; src = zr_reg; dst = zr_reg; tp = src_tp }
          ; Ast.Cmp { lhs = gen_value fun_name src; rhs = zr_reg; tp = src_tp }
          ; Ast.Mov { src = zr; dst; tp = dst_tp }
          ; Ast.SetC (E, dst)
          ]
        | _ ->
          [ Ast.Cmp { lhs = gen_value fun_name src; rhs = zr; tp = src_tp }
          ; Ast.Mov { src = zr; dst; tp = dst_tp }
          ; Ast.SetC (E, dst)
          ]))
  | _ -> assert false
;;

let classify_params fun_name params =
  let int_reg_args = Stack.create () in
  let double_reg_args = Stack.create () in
  let stack_args = Stack.create () in
  let loop_iter param =
    let oprnd = gen_value fun_name param in
    let tp = get_asm_type_for_val param in
    let typed_oprnd = oprnd, tp in
    if tp = AsmDouble
    then
      if Stack.length double_reg_args < 8
      then Stack.push oprnd double_reg_args
      else Stack.push typed_oprnd stack_args
    else if Stack.length int_reg_args < 6
    then Stack.push typed_oprnd int_reg_args
    else Stack.push typed_oprnd stack_args
  in
  List.iter loop_iter params;
  let reverser stk = Stack.fold (fun acc v -> v :: acc) [] stk in
  reverser int_reg_args, reverser double_reg_args, reverser stack_args
;;

let gen_instruction fun_name = function
  | Tacky.Ret v ->
    let tp = get_asm_type_for_val v in
    let dst = if tp = AsmDouble then Ast.Reg Ast.Xmm0 else Ast.Reg Ast.Ax in
    [ Ast.Mov { src = gen_value fun_name v; dst; tp }; Ast.Ret ]
  | Tacky.Unary _ as ret -> gen_ins_uop fun_name ret
  (* dst is always Tacky.Var *)
  | Tacky.Binary { bop; src1; src2; dst } as ret ->
    (match bop with
     | Tacky.Div | Tacky.Rem -> gen_ins_div fun_name ret
     | Tacky.Equal
     | Tacky.NEqual
     | Tacky.Less
     | Tacky.LEqual
     | Tacky.Greater
     | Tacky.GEqual ->
       let op_tp = get_asm_type_for_val src1 in
       let dst_tp = get_asm_type_for_val dst in
       assert (dst_tp = DWord);
       let signed =
         match op_tp with
         (* We treat floating point comparisons with unsigned logic because
            comisd sets CF and ZF flags. *)
         | AsmDouble -> false
         | _ -> signed src1
       in
       let dst = gen_value fun_name dst in
       let zr = Ast.Imm 0I in
       [ Ast.Cmp
           { lhs = gen_value fun_name src1; rhs = gen_value fun_name src2; tp = op_tp }
       ; Ast.Mov { src = zr; dst; tp = dst_tp }
       ; Ast.SetC (gen_cond_code signed bop, dst)
       ]
     | Tacky.Add | Tacky.Sub | Tacky.Mul | Tacky.And | Tacky.Or | Tacky.Xor ->
       let op_tp = get_asm_type_for_val dst in
       let dst = gen_value fun_name dst in
       [ Ast.Mov { src = gen_value fun_name src1; dst; tp = op_tp }
       ; Ast.Binary { bop = gen_bop bop; src = gen_value fun_name src2; dst; tp = op_tp }
       ]
     | Tacky.Lsft | Tacky.Rsft ->
       let op_tp = get_asm_type_for_val dst in
       let signed = signed dst in
       let src2 = gen_value fun_name src2 in
       let tmp_src2 = Ast.Reg Ast.Cx in
       let dst = gen_value fun_name dst in
       let bop = gen_bop bop in
       let bop = if (not signed) && bop = Ast.Sar then Ast.Shr else bop in
       [ Ast.Mov { src = gen_value fun_name src1; dst; tp = op_tp }
       ; Ast.Mov { src = src2; dst = tmp_src2; tp = op_tp }
       ; Ast.Binary { bop; src = tmp_src2; dst; tp = op_tp }
       ])
  | Tacky.Copy { src; dst } ->
    let tp = get_asm_type_for_val dst in
    [ Ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst; tp } ]
  | Tacky.Jump iden -> [ Ast.Jmp iden ]
  | Tacky.JumpIfZero (cnd, tgt) ->
    let tp = get_asm_type_for_val cnd in
    (match tp with
     | AsmDouble ->
       let zr_reg = Ast.Reg Ast.Xmm0 in
       [ Ast.Binary { bop = Ast.Xor; src = zr_reg; dst = zr_reg; tp }
       ; Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr_reg; tp }
       ; Ast.JmpC (Ast.E, tgt)
       ]
     | _ ->
       let zr = Ast.Imm 0I in
       [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; tp }; Ast.JmpC (Ast.E, tgt) ])
  | Tacky.JumpIfNotZero (cnd, tgt) ->
    let tp = get_asm_type_for_val cnd in
    (match tp with
     | AsmDouble ->
       let zr_reg = Ast.Reg Ast.Xmm0 in
       [ Ast.Binary { bop = Ast.Xor; src = zr_reg; dst = zr_reg; tp }
       ; Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr_reg; tp }
       ; Ast.JmpC (Ast.NE, tgt)
       ]
     | _ ->
       let zr = Ast.Imm 0I in
       [ Ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr; tp }; Ast.JmpC (Ast.NE, tgt) ])
  | Tacky.Label iden -> [ Ast.Label iden ]
  | Tacky.FunCall { name; args; dst } ->
    let int_args, double_args, stk_args = classify_params fun_name args in
    let stack_padding = if List.length stk_args mod 2 = 1 then 8 else 0 in
    let stack_alloc = if stack_padding <> 0 then [ alloc_stack_ins 8 ] else [] in
    let f reg_list ind (operand, tp) =
      Ast.Mov { src = operand; dst = Ast.Reg (List.nth reg_list ind); tp }
    in
    let int_reg_ins = List.mapi (f Ast.int_regs) int_args in
    let double_reg_ins =
      List.mapi
        (fun ind operand -> f Ast.double_regs ind (operand, AsmDouble))
        double_args
    in
    let f (operand, tp) =
      match tp with
      | QWord | AsmDouble -> [ Ast.Push operand ]
      | _ ->
        (match operand with
         | Ast.Imm _ | Ast.Reg _ -> [ Ast.Push operand ]
         | _ ->
           let dst = Ast.Reg Ast.Ax in
           [ Ast.Mov { src = operand; dst; tp }; Ast.Push dst ])
    in
    let stk_ins = List.concat_map f (List.rev stk_args) in
    let fun_call = [ Ast.Call name ] in
    let bytes_to_remove = (8 * List.length stk_args) + stack_padding in
    let stack_dealloc =
      if bytes_to_remove <> 0 then [ dealloc_stack_ins bytes_to_remove ] else []
    in
    let tp = AsmSymbolMap.get_fun_ret_type name in
    let dst = gen_value fun_name dst in
    let src = if tp = AsmDouble then Ast.Reg Ast.Xmm0 else Ast.Reg Ast.Ax in
    let ret_ins = [ Ast.Mov { src; dst; tp } ] in
    stack_alloc
    @ int_reg_ins
    @ double_reg_ins
    @ stk_ins
    @ fun_call
    @ stack_dealloc
    @ ret_ins
  | Tacky.SignExtend { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Movsx { src; dst } ]
  | Tacky.Truncate { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Mov { src; dst; tp = DWord } ]
  | Tacky.ZeroExtend { src; dst } ->
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.MovZeroExtend { src; dst } ]
  | Tacky.IntToDouble { src; dst } ->
    let src_tp = get_asm_type_for_val src in
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Cvtsi2sd { src; dst; src_tp } ]
  | Tacky.DoubleToInt { src; dst } ->
    let dst_tp = get_asm_type_for_val dst in
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    [ Ast.Cvttsd2si { src; dst; dst_tp } ]
  | Tacky.UIntToDouble { src; dst } ->
    let src_tp = get_asm_type_for_val src in
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    let label1 = Core.make_unique_label "UIntToDouble" in
    let label2 = Core.make_unique_label "UIntToDoubleEnd" in
    (match src_tp with
     | DWord ->
       let tmp_src = Ast.Reg Ast.Ax in
       [ Ast.MovZeroExtend { src; dst = tmp_src }
       ; Ast.Cvtsi2sd { src = tmp_src; dst; src_tp = QWord }
       ]
     | QWord ->
       let reg1 = Ast.Reg Ast.Ax in
       let reg2 = Ast.Reg Ast.Dx in
       [ Ast.Cmp { lhs = src; rhs = Ast.Imm 0I; tp = QWord }
       ; Ast.JmpC (Ast.L, Identifier label1)
       ; Ast.Cvtsi2sd { src; dst; src_tp = QWord }
       ; Ast.Jmp (Identifier label2)
       ; Ast.Label (Identifier label1)
       ; Ast.Mov { src; dst = reg1; tp = QWord }
       ; Ast.Mov { src = reg1; dst = reg2; tp = QWord }
       ; Ast.Binary { bop = Ast.Shr; src = Ast.Imm 1I; dst = reg2; tp = QWord }
       ; Ast.Binary { bop = Ast.And; src = Ast.Imm 1I; dst = reg1; tp = QWord }
       ; Ast.Binary { bop = Ast.Or; src = reg1; dst = reg2; tp = QWord }
       ; Ast.Cvtsi2sd { src = reg2; dst; src_tp = QWord }
       ; Ast.Binary { bop = Ast.Add; src = dst; dst; tp = AsmDouble }
       ; Ast.Label (Identifier label2)
       ]
     | _ -> assert false)
  | Tacky.DoubleToUInt { src; dst } ->
    let dst_tp = get_asm_type_for_val dst in
    let src = gen_value fun_name src in
    let dst = gen_value fun_name dst in
    (match dst_tp with
     | DWord ->
       let tmp_dst = Ast.Reg Ast.Ax in
       [ Ast.Cvttsd2si { src; dst = tmp_dst; dst_tp = QWord }
       ; Ast.Mov { src = tmp_dst; dst; tp = DWord }
       ]
     | QWord ->
       let regX = Ast.Reg Ast.Xmm0 in
       let regR = Ast.Reg Ast.Ax in
       let upper_bound = get_double_const 9223372036854775808.0 8 in
       let upper_bound_int = Stdint.Uint64.of_int64 9223372036854775808L in
       let label1 = Core.make_unique_label "DoubleToUInt" in
       let label2 = Core.make_unique_label "DoubleToUIntEnd" in
       [ Ast.Cmp { lhs = src; rhs = upper_bound; tp = AsmDouble }
       ; Ast.JmpC (Ast.AE, Identifier label1)
       ; Ast.Cvttsd2si { src; dst; dst_tp = QWord }
       ; Ast.Jmp (Identifier label2)
       ; Ast.Label (Identifier label1)
       ; Ast.Mov { src; dst = regX; tp = AsmDouble }
       ; Ast.Binary { bop = Ast.Sub; src = upper_bound; dst = regX; tp = AsmDouble }
       ; Ast.Cvttsd2si { src = regX; dst; dst_tp = QWord }
       ; Ast.Mov { src = Ast.Imm upper_bound_int; dst = regR; tp = QWord }
       ; Ast.Binary { bop = Ast.Add; src = regR; dst; tp = QWord }
       ; Ast.Label (Identifier label2)
       ]
     | _ -> assert false)
;;

let set_up_params fun_name params =
  let int_reg_params, double_reg_params, stk_params = classify_params fun_name params in
  let f reg_list ind (oprnd, tp) =
    Ast.Mov { src = Ast.Reg (List.nth reg_list ind); dst = oprnd; tp }
  in
  let int_reg_ins = List.mapi (f Ast.int_regs) int_reg_params in
  let double_reg_ins =
    List.mapi
      (fun ind oprnd -> f Ast.double_regs ind (oprnd, AsmDouble))
      double_reg_params
  in
  let stk_ins =
    List.mapi
      (fun ind (oprnd, tp) ->
         let src = Ast.Stack (-(16 + (8 * ind))) in
         Ast.Mov { src; dst = oprnd; tp })
      stk_params
  in
  int_reg_ins @ double_reg_ins @ stk_ins
;;

let gen_top_level = function
  | Tacky.Function { name; global; params; body } ->
    let params = List.map (fun param -> Tacky.Var param) params in
    let param_ins = set_up_params name params in
    let body = param_ins @ List.concat_map (gen_instruction name) body in
    let alloc_stack = get_fun_stack_alloc name in
    let align16 = if alloc_stack > 0 then 16 * ((alloc_stack / 16) + 1) else 0 in
    let body = if align16 > 0 then alloc_stack_ins align16 :: body else body in
    let body = List.concat_map Ins_fixer.fix_instruction body in
    Ast.Function { name; global; body }
  | Tacky.StaticVar { name; global; tp; init } ->
    let alignment =
      match AsmSymbolMap.get_asm_type_for_c_type tp with
      | Byte -> 1
      | Word -> 2
      | DWord -> 4
      | QWord -> 8
      | AsmDouble -> 8
    in
    Ast.StaticVar { name; global; alignment; init }
;;

let gen_program = function
  | Tacky.Program tpns ->
    let tpns = List.map gen_top_level tpns in
    let acc =
      Hashtbl.fold
        (fun (ci, alignment) lbl acc ->
           let tp_ast =
             Ast.StaticConstant
               { name = Identifier lbl
               ; alignment
               ; init = ConstDouble (Int64.float_of_bits ci)
               }
           in
           tp_ast :: acc)
        static_consts
        tpns
    in
    Ast.Program acc
;;
