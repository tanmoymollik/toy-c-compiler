let gen_identifier = function
  | C_ast.Identifier ident -> Tacky.Identifier ident
;;

let gen_uop = function
  | C_ast.Complement -> Tacky.Complement
  | C_ast.Negate -> Tacky.Negate
;;

let tmp_var_count = ref 0

let make_tmp_dst () =
  let c = !tmp_var_count in
  tmp_var_count := c + 1;
  Tacky.Var (Tacky.Identifier (Printf.sprintf "tmp.%d" c))
;;

let rec gen_expression ins = function
  | C_ast.Constant c -> Tacky.Constant c
  | C_ast.Unary (uop, exp) ->
    let src = gen_expression ins exp in
    let uop = gen_uop uop in
    let dst = make_tmp_dst () in
    let u_ins = Tacky.Unary { uop; src; dst } in
    Stack.push u_ins ins;
    dst
;;

let gen_statement = function
  | C_ast.Return exp ->
    let stk = Stack.create () in
    let exp_val = gen_expression stk exp in
    Stack.push (Tacky.Ret exp_val) stk;
    let f acc a = a :: acc in
    Stack.fold f [] stk
;;

let gen_function_def = function
  | C_ast.Function f ->
    Tacky.Function { name = gen_identifier f.name; body = gen_statement f.body }
;;

let gen_program = function
  | C_ast.Program f -> Tacky.Program (gen_function_def f)
;;
