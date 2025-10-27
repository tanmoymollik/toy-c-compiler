let gen_identifier = function
  | C_ast.Identifier ident -> X64_ast.Identifier ident
;;

let gen_expression = function
  | C_ast.Constant c -> X64_ast.Imm c
;;

let gen_statement = function
  | C_ast.Return exp ->
    [ X64_ast.Mov { src = gen_expression exp; dst = X64_ast.Register }; X64_ast.Ret ]
;;

let gen_function_def = function
  | C_ast.Function f ->
    X64_ast.Function { name = gen_identifier f.name; body = gen_statement f.body }
;;

let gen_program = function
  | C_ast.Program f -> X64_ast.Program (gen_function_def f)
;;
