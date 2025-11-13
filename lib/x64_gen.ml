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
    let stk_ptr = get_fun_stack_alloc fun_name + 4 in
    Hashtbl.add fun_map fun_name stk_ptr;
    Hashtbl.add var_map iden stk_ptr;
    stk_ptr
;;

let gen_identifier = function
  | Tacky.Identifier ident -> X64_ast.Identifier ident
;;

let gen_uop = function
  | Tacky.Complement -> X64_ast.Not
  | Tacky.Negate -> X64_ast.Neg
  (* Handled differently. *)
  | _ -> assert false
;;

let gen_bop = function
  | Tacky.Add -> X64_ast.Add
  | Tacky.Sub -> X64_ast.Sub
  | Tacky.Mul -> X64_ast.Imul
  | Tacky.And -> X64_ast.And
  | Tacky.Or -> X64_ast.Or
  | Tacky.Xor -> X64_ast.Xor
  | Tacky.Lsft -> X64_ast.Sal
  | Tacky.Rsft -> X64_ast.Sar
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

let gen_cond_code = function
  | Tacky.Equal -> X64_ast.E
  | Tacky.NEqual -> X64_ast.NE
  | Tacky.Less -> X64_ast.L
  | Tacky.LEqual -> X64_ast.LE
  | Tacky.Greater -> X64_ast.G
  | Tacky.GEqual -> X64_ast.GE
  (* Not conditional ops. *)
  | _ -> assert false
;;

let gen_stack_for_var fun_name = function
  | Tacky.Identifier iden ->
    let addr = get_stack_address fun_name iden in
    X64_ast.Stack addr, X64_ast.DWord
;;

let gen_value fun_name = function
  | Tacky.Constant c -> X64_ast.Imm c, X64_ast.DWord
  | Tacky.Var var -> gen_stack_for_var fun_name var
;;

let gen_instruction fun_name = function
  | Tacky.Ret v ->
    [ X64_ast.Mov
        { src = gen_value fun_name v; dst = X64_ast.Reg X64_ast.Ax, X64_ast.DWord }
    ; X64_ast.Ret
    ]
  | Tacky.Unary { dst = Tacky.Constant _; _ } -> assert false
  (* dst is always Tacky.Var *)
  | Tacky.Unary { uop; src; dst } ->
    (match uop with
     | Tacky.Complement | Tacky.Negate ->
       [ X64_ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst }
       ; X64_ast.Unary (gen_uop uop, gen_value fun_name dst)
       ]
     | Tacky.Not ->
       let dst = gen_value fun_name dst in
       let zr = X64_ast.Imm 0, X64_ast.DWord in
       [ X64_ast.Cmp { lhs = gen_value fun_name src; rhs = zr }
       ; X64_ast.Mov { src = zr; dst }
       ; X64_ast.SetC (E, dst)
       ])
  | Tacky.Binary { dst = Tacky.Constant _; _ } -> assert false
  (* dst is always Tacky.Var *)
  | Tacky.Binary { bop; src1; src2; dst } ->
    (match bop with
     | Tacky.Div ->
       let tmp_dst = X64_ast.Reg X64_ast.Ax, X64_ast.DWord in
       [ X64_ast.Mov { src = gen_value fun_name src1; dst = tmp_dst }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value fun_name src2)
       ; X64_ast.Mov { src = tmp_dst; dst = gen_value fun_name dst }
       ]
     | Tacky.Rem ->
       [ X64_ast.Mov
           { src = gen_value fun_name src1; dst = X64_ast.Reg X64_ast.Ax, X64_ast.DWord }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value fun_name src2)
       ; X64_ast.Mov
           { src = X64_ast.Reg X64_ast.Dx, X64_ast.DWord; dst = gen_value fun_name dst }
       ]
     | Tacky.Equal
     | Tacky.NEqual
     | Tacky.Less
     | Tacky.LEqual
     | Tacky.Greater
     | Tacky.GEqual ->
       let dst = gen_value fun_name dst in
       let zr = X64_ast.Imm 0, X64_ast.DWord in
       [ X64_ast.Cmp { lhs = gen_value fun_name src1; rhs = gen_value fun_name src2 }
       ; X64_ast.Mov { src = zr; dst }
       ; X64_ast.SetC (gen_cond_code bop, dst)
       ]
     | Tacky.Add | Tacky.Sub | Tacky.Mul | Tacky.And | Tacky.Or | Tacky.Xor ->
       let dst = gen_value fun_name dst in
       [ X64_ast.Mov { src = gen_value fun_name src1; dst }
       ; X64_ast.Binary { bop = gen_bop bop; src = gen_value fun_name src2; dst }
       ]
     | Tacky.Lsft | Tacky.Rsft ->
       let src2_t, src2_s = gen_value fun_name src2 in
       let tmp_src2_t = X64_ast.Reg X64_ast.Cx in
       let dst = gen_value fun_name dst in
       [ X64_ast.Mov { src = gen_value fun_name src1; dst }
       ; X64_ast.Mov { src = src2_t, src2_s; dst = tmp_src2_t, src2_s }
       ; X64_ast.Binary { bop = gen_bop bop; src = tmp_src2_t, X64_ast.Byte; dst }
       ])
  | Tacky.Copy { src; dst } ->
    [ X64_ast.Mov { src = gen_value fun_name src; dst = gen_value fun_name dst } ]
  | Tacky.Jump iden -> [ X64_ast.Jmp (gen_identifier iden) ]
  | Tacky.JumpIfZero (cnd, tgt) ->
    let zr = X64_ast.Imm 0, X64_ast.DWord in
    [ X64_ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr }
    ; X64_ast.JmpC (X64_ast.E, gen_identifier tgt)
    ]
  | Tacky.JumpIfNotZero (cnd, tgt) ->
    let zr = X64_ast.Imm 0, X64_ast.DWord in
    [ X64_ast.Cmp { lhs = gen_value fun_name cnd; rhs = zr }
    ; X64_ast.JmpC (X64_ast.NE, gen_identifier tgt)
    ]
  | Tacky.Label iden -> [ X64_ast.Label (gen_identifier iden) ]
  | Tacky.FunCall { name; args; dst } ->
    let arg_regs = X64_ast.arg_regs in
    let arg_regs_len = X64_ast.arg_regs_len in
    let reg_args, stk_args = List.take arg_regs_len args, List.drop arg_regs_len args in
    let stack_padding = if List.length stk_args mod 2 = 1 then 8 else 0 in
    let stack_alloc = if stack_padding <> 0 then [ X64_ast.AllocStack 8 ] else [] in
    let f ind arg =
      let src_t, src_s = gen_value fun_name arg in
      let dst = X64_ast.Reg (List.nth arg_regs ind), src_s in
      X64_ast.Mov { src = src_t, src_s; dst }
    in
    let reg_arg_ins = List.mapi f reg_args in
    let f arg =
      let src_t, src_s = gen_value fun_name arg in
      match src_t with
      | X64_ast.Imm _ | X64_ast.Reg _ -> [ X64_ast.Push (src_t, X64_ast.QWord) ]
      | _ ->
        let dst_t = X64_ast.Reg X64_ast.Ax in
        [ X64_ast.Mov { src = src_t, src_s; dst = dst_t, src_s }
        ; X64_ast.Push (dst_t, X64_ast.QWord)
        ]
    in
    let stk_arg_ins = List.concat_map f (List.rev stk_args) in
    let fun_call = [ X64_ast.Call (gen_identifier name) ] in
    let bytes_to_remove = (8 * List.length stk_args) + stack_padding in
    let stack_dealloc =
      if bytes_to_remove <> 0 then [ X64_ast.DeallocStack bytes_to_remove ] else []
    in
    let dst_t, dst_s = gen_value fun_name dst in
    let src = X64_ast.Reg X64_ast.Ax, dst_s in
    let ret_ins = [ X64_ast.Mov { src; dst = dst_t, dst_s } ] in
    stack_alloc @ reg_arg_ins @ stk_arg_ins @ fun_call @ stack_dealloc @ ret_ins
;;

let fix_ins_cmp = function
  (* It is helpful to consider lhs as dst and rhs as src to better understand the semantics. *)
  | X64_ast.Cmp { lhs; rhs } as ret ->
    (match lhs, rhs with
     | (X64_ast.Imm _, sz), _ ->
       let tmp_lhs = X64_ast.Reg X64_ast.R11, sz in
       [ X64_ast.Mov { src = lhs; dst = tmp_lhs }; X64_ast.Cmp { lhs = tmp_lhs; rhs } ]
     | _, (Stack _, sz) ->
       let tmp_rhs = X64_ast.Reg X64_ast.R10, sz in
       [ X64_ast.Mov { src = rhs; dst = tmp_rhs }; X64_ast.Cmp { lhs; rhs = tmp_rhs } ]
     | _ -> [ ret ])
  | _ -> assert false
;;

let fix_instruction = function
  | X64_ast.Mov { src; dst } as ret ->
    (match src, dst with
     | (X64_ast.Stack _, sz), (X64_ast.Stack _, _) ->
       let tmp_src = X64_ast.Reg X64_ast.R10, sz in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Mov { src = tmp_src; dst } ]
     | _ -> [ ret ])
  | X64_ast.Cmp _ as ret -> fix_ins_cmp ret
  | X64_ast.Binary { bop; src; dst } as ret ->
    (match bop, src, dst with
     | ( (X64_ast.Add | X64_ast.Sub | X64_ast.And | X64_ast.Or | X64_ast.Xor)
       , (X64_ast.Stack _, sz)
       , (X64_ast.Stack _, _) ) ->
       let tmp_src = X64_ast.Reg X64_ast.R10, sz in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Binary { bop; src = tmp_src; dst } ]
     | X64_ast.Imul, _, (X64_ast.Stack _, sz) ->
       let tmp_dst = X64_ast.Reg X64_ast.R11, sz in
       [ X64_ast.Mov { src = dst; dst = tmp_dst }
       ; X64_ast.Binary { bop; src; dst = tmp_dst }
       ; X64_ast.Mov { src = tmp_dst; dst }
       ]
     | _, _, _ -> [ ret ])
  | X64_ast.Idiv src as ret ->
    (match src with
     | X64_ast.Imm _, sz ->
       let tmp_src = X64_ast.Reg X64_ast.R10, sz in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Idiv tmp_src ]
     | _ -> [ ret ])
  | _ as ret -> [ ret ]
;;

let gen_function_def = function
  | Tacky.Function { name = Tacky.Identifier name; params; body } ->
    let arg_regs = X64_ast.arg_regs in
    let arg_regs_len = X64_ast.arg_regs_len in
    let reg_params, stk_params =
      List.take arg_regs_len params, List.drop arg_regs_len params
    in
    let f ind arg =
      let dst_t, dst_s = gen_stack_for_var name arg in
      let src_t = X64_ast.Reg (List.nth arg_regs ind) in
      X64_ast.Mov { src = src_t, dst_s; dst = dst_t, dst_s }
    in
    let reg_param_ins = List.mapi f reg_params in
    let f ind arg =
      let dst_t, _ = gen_stack_for_var name arg in
      let src_t = X64_ast.Stack (-(16 + (8 * ind))) in
      let src_s = X64_ast.DWord in
      X64_ast.Mov { src = src_t, src_s; dst = dst_t, src_s }
    in
    let stk_param_ins = List.mapi f stk_params in
    let body =
      reg_param_ins @ stk_param_ins @ List.concat_map (gen_instruction name) body
    in
    let alloc_stack = get_fun_stack_alloc name in
    let alloc_stack = if alloc_stack > 0 then 16 * ((alloc_stack / 16) + 1) else 0 in
    let ins = if alloc_stack > 0 then X64_ast.AllocStack alloc_stack :: body else body in
    let body = List.concat_map fix_instruction ins in
    X64_ast.Function { name = X64_ast.Identifier name; body }
;;

let gen_program = function
  | Tacky.Program fns -> X64_ast.Program (List.map gen_function_def fns)
;;
