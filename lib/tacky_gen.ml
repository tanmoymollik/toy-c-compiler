let gen_identifier = function
  | C_ast.Identifier iden -> Tacky.Identifier iden
;;

let gen_uop = function
  | C_ast.Complement -> Tacky.Complement
  | C_ast.Negate -> Tacky.Negate
  | C_ast.Not -> Tacky.Not
;;

let gen_bop = function
  | C_ast.Add -> Tacky.Add
  | C_ast.Sub -> Tacky.Sub
  | C_ast.Mul -> Tacky.Mul
  | C_ast.Div -> Tacky.Div
  | C_ast.Rem -> Tacky.Rem
  | C_ast.BAnd -> Tacky.And
  | C_ast.BOr -> Tacky.Or
  | C_ast.Xor -> Tacky.Xor
  | C_ast.Lsft -> Tacky.Lsft
  | C_ast.Rsft -> Tacky.Rsft
  | C_ast.Equal -> Tacky.Equal
  | C_ast.NEqual -> Tacky.NEqual
  | C_ast.Less -> Tacky.Less
  | C_ast.LEqual -> Tacky.LEqual
  | C_ast.GEqual -> Tacky.GEqual
  | C_ast.Greater -> Tacky.Greater
  (* And and Or handled differently *)
  | C_ast.And | C_ast.Or -> assert false
;;

let tmp_var_count = ref 0

let make_tmp_dst () =
  let c = !tmp_var_count in
  tmp_var_count := c + 1;
  Tacky.Var (Tacky.Identifier (Printf.sprintf "tmp.%d" c))
;;

let label_count = ref 0

let make_local_label prefix =
  let c = !label_count in
  label_count := c + 1;
  Tacky.Identifier (Printf.sprintf "%s_%d" prefix c)
;;

let rec gen_expression ins = function
  | C_ast.Constant c -> Tacky.Constant c
  | C_ast.Unary (uop, exp) ->
    let uop = gen_uop uop in
    let src = gen_expression ins exp in
    let dst = make_tmp_dst () in
    let u_ins = Tacky.Unary { uop; src; dst } in
    Stack.push u_ins ins;
    dst
  | C_ast.Binary { bop; lexp; rexp } ->
    (match bop with
     | C_ast.And | C_ast.Or ->
       let cnd1 = gen_expression ins lexp in
       let br_label = make_local_label "cnd_branch" in
       let en_label = make_local_label "end_branch" in
       (* Default short-circuit value. *)
       let dflt = if bop = C_ast.And then 0 else 1 in
       let j1 =
         if bop = C_ast.And
         then Tacky.JumpIfZero (cnd1, br_label)
         else Tacky.JumpIfNotZero (cnd1, br_label)
       in
       Stack.push j1 ins;
       let cnd2 = gen_expression ins rexp in
       let j2 =
         if bop = C_ast.And
         then Tacky.JumpIfZero (cnd2, br_label)
         else Tacky.JumpIfNotZero (cnd2, br_label)
       in
       Stack.push j2 ins;
       let dst = make_tmp_dst () in
       Stack.push (Tacky.Copy { src = Tacky.Constant (1 lxor dflt); dst }) ins;
       Stack.push (Tacky.Jump en_label) ins;
       Stack.push (Tacky.Label br_label) ins;
       Stack.push (Tacky.Copy { src = Tacky.Constant dflt; dst }) ins;
       Stack.push (Tacky.Label en_label) ins;
       dst
     | _ ->
       let bop = gen_bop bop in
       let src1 = gen_expression ins lexp in
       let src2 = gen_expression ins rexp in
       let dst = make_tmp_dst () in
       let b_ins = Tacky.Binary { bop; src1; src2; dst } in
       Stack.push b_ins ins;
       dst)
;;

let gen_statement = function
  | C_ast.Return exp ->
    let stk = Stack.create () in
    let exp_val = gen_expression stk exp in
    Stack.push (Tacky.Ret exp_val) stk;
    (* The stack is effectively reversed here. *)
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
