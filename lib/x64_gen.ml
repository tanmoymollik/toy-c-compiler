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

let alloc_stack = ref 0

let gen_stack_address ident =
  let parts = String.split_on_char '.' ident in
  assert (List.length parts = 2);
  let idx = int_of_string (List.nth parts 1) in
  4 * (idx + 1)
;;

let gen_value = function
  | Tacky.Constant c -> X64_ast.Imm c
  | Tacky.Var (Tacky.Identifier ident) ->
    let addr = gen_stack_address ident in
    alloc_stack := max !alloc_stack addr;
    X64_ast.Stack addr
;;

let gen_instruction = function
  | Tacky.Ret v ->
    [ X64_ast.Mov { src = gen_value v; dst = X64_ast.Reg X64_ast.Ax }; X64_ast.Ret ]
  | Tacky.Unary { dst = Tacky.Constant _; _ } -> assert false
  (* dst is always Tacky.Var *)
  | Tacky.Unary { uop; src; dst } ->
    (match uop with
     | Tacky.Complement | Tacky.Negate ->
       [ X64_ast.Mov { src = gen_value src; dst = gen_value dst }
       ; X64_ast.Unary (gen_uop uop, gen_value dst)
       ]
     | Tacky.Not ->
       let dst = gen_value dst in
       let zr = X64_ast.Imm 0 in
       [ X64_ast.Cmp { lhs = gen_value src; rhs = zr }
       ; X64_ast.Mov { src = zr; dst }
       ; X64_ast.SetC (E, dst)
       ])
  | Tacky.Binary { dst = Tacky.Constant _; _ } -> assert false
  (* dst is always Tacky.Var *)
  | Tacky.Binary { bop; src1; src2; dst } ->
    (match bop with
     | Tacky.Div ->
       let tmp_dst = X64_ast.Reg X64_ast.Ax in
       [ X64_ast.Mov { src = gen_value src1; dst = tmp_dst }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value src2)
       ; X64_ast.Mov { src = tmp_dst; dst = gen_value dst }
       ]
     | Tacky.Rem ->
       [ X64_ast.Mov { src = gen_value src1; dst = X64_ast.Reg X64_ast.Ax }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value src2)
       ; X64_ast.Mov { src = X64_ast.Reg X64_ast.Dx; dst = gen_value dst }
       ]
     | Tacky.Equal
     | Tacky.NEqual
     | Tacky.Less
     | Tacky.LEqual
     | Tacky.Greater
     | Tacky.GEqual ->
       let dst = gen_value dst in
       [ X64_ast.Cmp { lhs = gen_value src1; rhs = gen_value src2 }
       ; X64_ast.Mov { src = X64_ast.Imm 0; dst }
       ; X64_ast.SetC (gen_cond_code bop, dst)
       ]
     | _ ->
       [ X64_ast.Mov { src = gen_value src1; dst = gen_value dst }
       ; X64_ast.Binary { bop = gen_bop bop; src = gen_value src2; dst = gen_value dst }
       ])
  | Tacky.Copy { src; dst } ->
    [ X64_ast.Mov { src = gen_value src; dst = gen_value dst } ]
  | Tacky.Jump iden -> [ X64_ast.Jmp (gen_identifier iden) ]
  | Tacky.JumpIfZero (cnd, tgt) ->
    [ X64_ast.Cmp { lhs = gen_value cnd; rhs = X64_ast.Imm 0 }
    ; X64_ast.JmpC (X64_ast.E, gen_identifier tgt)
    ]
  | Tacky.JumpIfNotZero (cnd, tgt) ->
    [ X64_ast.Cmp { lhs = gen_value cnd; rhs = X64_ast.Imm 0 }
    ; X64_ast.JmpC (X64_ast.NE, gen_identifier tgt)
    ]
  | Tacky.Label iden -> [ X64_ast.Label (gen_identifier iden) ]
  | Tacky.FunCall _ -> assert false
;;

let fix_ins_cmp = function
  (* It is helpful to consider lhs as dst and rhs as src to better understand the semantics. *)
  | X64_ast.Cmp { lhs; rhs } as ret ->
    (match lhs, rhs with
     | X64_ast.Imm _, _ ->
       let tmp_lhs = X64_ast.Reg X64_ast.R11 in
       [ X64_ast.Mov { src = lhs; dst = tmp_lhs }; X64_ast.Cmp { lhs = tmp_lhs; rhs } ]
     | _, Stack _ ->
       let tmp_rhs = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src = rhs; dst = tmp_rhs }; X64_ast.Cmp { lhs; rhs = tmp_rhs } ]
     | _ -> [ ret ])
  | _ -> assert false
;;

let fix_instruction = function
  | X64_ast.Mov { dst = X64_ast.Imm _; _ } -> assert false
  | X64_ast.Mov { src; dst } as ret ->
    (match src, dst with
     | X64_ast.Stack _, X64_ast.Stack _ ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Mov { src = tmp_src; dst } ]
     | _ -> [ ret ])
  | X64_ast.Cmp _ as ret -> fix_ins_cmp ret
  | X64_ast.Binary { bop; src; dst } as ret ->
    (match bop, src, dst with
     | ( (X64_ast.Add | X64_ast.Sub | X64_ast.And | X64_ast.Or | X64_ast.Xor)
       , X64_ast.Stack _
       , X64_ast.Stack _ ) ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Binary { bop; src = tmp_src; dst } ]
     | (X64_ast.Sal | X64_ast.Sar), X64_ast.Stack _, X64_ast.Stack _ ->
       (* We don't have different sized registers yet. For now we copy to Cx
             and use Cl when shifting. *)
       [ X64_ast.Mov { src; dst = X64_ast.Reg X64_ast.Cx }
       ; X64_ast.Binary { bop; src = X64_ast.Reg X64_ast.Cl; dst }
       ]
     | X64_ast.Imul, _, X64_ast.Stack _ ->
       let tmp_dst = X64_ast.Reg X64_ast.R11 in
       [ X64_ast.Mov { src = dst; dst = tmp_dst }
       ; X64_ast.Binary { bop; src; dst = tmp_dst }
       ; X64_ast.Mov { src = tmp_dst; dst }
       ]
     | _, _, _ -> [ ret ])
  | X64_ast.Idiv src as ret ->
    (match src with
     | X64_ast.Imm _ ->
       let dst = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst }; X64_ast.Idiv dst ]
     | _ -> [ ret ])
  | _ as ret -> [ ret ]
;;

let gen_function_def = function
  | Tacky.Function f ->
    let ins = List.concat_map gen_instruction f.body in
    let ins = if !alloc_stack <> 0 then X64_ast.AllocStack !alloc_stack :: ins else ins in
    let body = List.concat_map fix_instruction ins in
    X64_ast.Function { name = gen_identifier f.name; body }
;;

let gen_program = function
  | Tacky.Program fns -> X64_ast.Program (List.map gen_function_def fns)
;;
