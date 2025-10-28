let gen_identifier = function
  | Tacky.Identifier ident -> X64_ast.Identifier ident
;;

let gen_uop = function
  | Tacky.Complement -> X64_ast.Not
  | Tacky.Negate -> X64_ast.Neg
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
  | Tacky.Ret v ->
    [ X64_ast.Mov { src = gen_value v; dst = X64_ast.Reg X64_ast.AX }; X64_ast.Ret ]
;;

let fix_instruction = function
  | X64_ast.Mov { src; dst } as ret ->
    (match dst with
     | X64_ast.Stack _ ->
       [ X64_ast.Mov { src; dst = X64_ast.Reg X64_ast.R10 }
       ; X64_ast.Mov { src = X64_ast.Reg X64_ast.R10; dst }
       ]
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
