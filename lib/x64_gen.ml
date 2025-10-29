let gen_identifier = function
  | Tacky.Identifier ident -> X64_ast.Identifier ident
;;

let gen_uop = function
  | Tacky.Complement -> X64_ast.Not
  | Tacky.Negate -> X64_ast.Neg
;;

let gen_bop = function
  | Tacky.Add -> X64_ast.Add
  | Tacky.Sub -> X64_ast.Sub
  | Tacky.Mul -> X64_ast.Mul
  | _ -> assert false
;;

let alloc_stack = ref 0

let gen_stack_address ident =
  let parts = String.split_on_char '.' ident in
  assert (List.length parts = 2);
  assert (List.hd parts = "tmp");
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
  | Tacky.Unary { uop; src; dst } ->
    [ X64_ast.Mov { src = gen_value src; dst = gen_value dst }
    ; X64_ast.Unary (gen_uop uop, gen_value dst)
    ]
  | Tacky.Binary { bop; src1; src2; dst } ->
    (match bop with
     | (Tacky.Add | Tacky.Sub | Tacky.Mul) as bop ->
       [ X64_ast.Mov { src = gen_value src1; dst = gen_value dst }
       ; X64_ast.Binary { bop = gen_bop bop; src = gen_value src2; dst = gen_value dst }
       ]
     | Tacky.Div ->
       [ X64_ast.Mov { src = gen_value src1; dst = X64_ast.Reg X64_ast.AX }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value src2)
       ; X64_ast.Mov { src = X64_ast.Reg X64_ast.AX; dst = gen_value dst }
       ]
     | Tacky.Rem ->
       [ X64_ast.Mov { src = gen_value src1; dst = X64_ast.Reg X64_ast.AX }
       ; X64_ast.Cdq
       ; X64_ast.Idiv (gen_value src2)
       ; X64_ast.Mov { src = X64_ast.Reg X64_ast.DX; dst = gen_value dst }
       ])
  | Tacky.Ret v ->
    [ X64_ast.Mov { src = gen_value v; dst = X64_ast.Reg X64_ast.AX }; X64_ast.Ret ]
;;

let fix_instruction = function
  | X64_ast.Mov { src; dst } as ret ->
    (match src with
     | X64_ast.Stack _ ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src }; X64_ast.Mov { src = tmp_src; dst } ]
     | _ -> [ ret ])
  | X64_ast.Binary { bop; src; dst } as ret ->
    (match bop with
     | X64_ast.Add | X64_ast.Sub ->
       (match src with
        | X64_ast.Stack _ ->
          let tmp_src = X64_ast.Reg X64_ast.R10 in
          [ X64_ast.Mov { src; dst = tmp_src }
          ; X64_ast.Binary { bop; src = tmp_src; dst }
          ]
        | _ -> [ ret ])
     | X64_ast.Mul ->
       (match dst with
        | X64_ast.Stack _ ->
          let tmp_dst = X64_ast.Reg X64_ast.R11 in
          [ X64_ast.Mov { src = dst; dst = tmp_dst }
          ; X64_ast.Binary { bop; src; dst = tmp_dst }
          ; X64_ast.Mov { src = tmp_dst; dst }
          ]
        | _ -> [ ret ]))
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
  | Tacky.Program f -> X64_ast.Program (gen_function_def f)
;;
