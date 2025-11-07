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

let aop_to_bop = function
  | C_ast.AEq -> Tacky.Add
  | C_ast.SEq -> Tacky.Sub
  | C_ast.MEq -> Tacky.Mul
  | C_ast.DEq -> Tacky.Div
  | C_ast.REq -> Tacky.Rem
  | C_ast.BAEq -> Tacky.And
  | C_ast.BOEq -> Tacky.Or
  | C_ast.XEq -> Tacky.Xor
  | C_ast.LsftEq -> Tacky.Lsft
  | C_ast.RsftEq -> Tacky.Rsft
  | C_ast.Eq -> assert false
;;

let make_tmp_dst () =
  let c = Core.get_var_count () in
  Tacky.Var (Tacky.Identifier (Printf.sprintf "tmp.%d" c))
;;

let make_label prefix = Tacky.Identifier (Core.make_unique_label prefix)

let rec gen_expression stk = function
  | C_ast.Constant c -> Tacky.Constant c
  | C_ast.Var iden -> Tacky.Var (gen_identifier iden)
  | C_ast.Unary (uop, exp) ->
    let uop = gen_uop uop in
    let src = gen_expression stk exp in
    let dst = make_tmp_dst () in
    let u_ins = Tacky.Unary { uop; src; dst } in
    Stack.push u_ins stk;
    dst
  | C_ast.TUnary (tuop, prefix, lval) ->
    (match lval with
     | C_ast.Var _ ->
       let src = gen_expression stk lval in
       let bop =
         match tuop with
         | C_ast.Inc -> Tacky.Add
         | C_ast.Dec -> Tacky.Sub
       in
       let i = Tacky.Binary { bop; src1 = src; src2 = Tacky.Constant 1; dst = src } in
       let dst = make_tmp_dst () in
       if not prefix then Stack.push (Tacky.Copy { src; dst }) stk;
       Stack.push i stk;
       if prefix then src else dst
     | _ -> assert false)
  | C_ast.Binary { bop; lexp; rexp } ->
    (match bop with
     | C_ast.And | C_ast.Or ->
       let cnd1 = gen_expression stk lexp in
       let br_label = make_label ("_other" ^ Core.binary_label) in
       let en_label = make_label ("_end" ^ Core.binary_label) in
       (* Default short-circuit value. *)
       let dflt = if bop = C_ast.And then 0 else 1 in
       let j1 =
         if bop = C_ast.And
         then Tacky.JumpIfZero (cnd1, br_label)
         else Tacky.JumpIfNotZero (cnd1, br_label)
       in
       Stack.push j1 stk;
       let cnd2 = gen_expression stk rexp in
       let j2 =
         if bop = C_ast.And
         then Tacky.JumpIfZero (cnd2, br_label)
         else Tacky.JumpIfNotZero (cnd2, br_label)
       in
       Stack.push j2 stk;
       let dst = make_tmp_dst () in
       Stack.push (Tacky.Copy { src = Tacky.Constant (1 lxor dflt); dst }) stk;
       Stack.push (Tacky.Jump en_label) stk;
       Stack.push (Tacky.Label br_label) stk;
       Stack.push (Tacky.Copy { src = Tacky.Constant dflt; dst }) stk;
       Stack.push (Tacky.Label en_label) stk;
       dst
     | _ ->
       let bop = gen_bop bop in
       let src1 = gen_expression stk lexp in
       let src2 = gen_expression stk rexp in
       let dst = make_tmp_dst () in
       let b_ins = Tacky.Binary { bop; src1; src2; dst } in
       Stack.push b_ins stk;
       dst)
  | C_ast.Assignment { aop; lval; rval } ->
    (match lval with
     | C_ast.Var _ ->
       let src = gen_expression stk rval in
       let dst = gen_expression stk lval in
       (match aop with
        | C_ast.Eq -> Stack.push (Tacky.Copy { src; dst }) stk
        | aop ->
          let bop = aop_to_bop aop in
          Stack.push (Tacky.Binary { bop; src1 = dst; src2 = src; dst }) stk);
       dst
     | _ -> assert false)
  | C_ast.Conditional { cnd; lhs; rhs } ->
    let cnd = gen_expression stk cnd in
    let rhs_lbl = make_label ("_other" ^ Core.conditional_label) in
    let en_lbl = make_label ("_end" ^ Core.conditional_label) in
    let dst = make_tmp_dst () in
    Stack.push (Tacky.JumpIfZero (cnd, rhs_lbl)) stk;
    let src = gen_expression stk lhs in
    Stack.push (Tacky.Copy { src; dst }) stk;
    Stack.push (Tacky.Jump en_lbl) stk;
    Stack.push (Tacky.Label rhs_lbl) stk;
    let src = gen_expression stk rhs in
    Stack.push (Tacky.Copy { src; dst }) stk;
    Stack.push (Tacky.Label en_lbl) stk;
    dst
;;

let gen_declaration stk = function
  | C_ast.Declaration { name; init = Some exp } ->
    let exp_val = gen_expression stk exp in
    Stack.push (Tacky.Copy { src = exp_val; dst = Tacky.Var (gen_identifier name) }) stk
  | _ -> ()
;;

let gen_for_init stk = function
  | C_ast.InitDecl d -> gen_declaration stk d
  | C_ast.InitExp e ->
    (match e with
     | Some e ->
       let _ = gen_expression stk e in
       ()
     | None -> ())
;;

let rec gen_statement stk = function
  | C_ast.Return exp ->
    let exp_val = gen_expression stk exp in
    Stack.push (Tacky.Ret exp_val) stk
  | C_ast.Expression exp ->
    let _ = gen_expression stk exp in
    ()
  | C_ast.If { cnd; thn; els } ->
    let cnd = gen_expression stk cnd in
    let els_lbl =
      if els != None
      then make_label ("_else" ^ Core.if_label)
      else Tacky.Identifier "not_used"
    in
    let en_lbl = make_label ("_end" ^ Core.if_label) in
    let to_go = if els != None then els_lbl else en_lbl in
    Stack.push (Tacky.JumpIfZero (cnd, to_go)) stk;
    let () = gen_statement stk thn in
    if els != None
    then (
      Stack.push (Tacky.Jump en_lbl) stk;
      Stack.push (Tacky.Label els_lbl) stk;
      let _ = Option.map (gen_statement stk) els in
      ())
    else ();
    Stack.push (Tacky.Label en_lbl) stk
  | C_ast.Goto label -> Stack.push (Tacky.Jump (gen_identifier label)) stk
  | C_ast.Label (label, stmt) ->
    Stack.push (Tacky.Label (gen_identifier label)) stk;
    gen_statement stk stmt
  | C_ast.Compound block -> gen_block stk block
  | C_ast.Break (C_ast.Identifier label) ->
    let label = Tacky.Identifier (Core.break_label label) in
    Stack.push (Tacky.Jump label) stk;
    ()
  | C_ast.Continue (C_ast.Identifier label) ->
    let label = Tacky.Identifier (Core.continue_label label) in
    Stack.push (Tacky.Jump label) stk;
    ()
  | C_ast.While (exp, stmt, C_ast.Identifier label) ->
    let cont_label = Tacky.Identifier (Core.continue_label label) in
    let brk_label = Tacky.Identifier (Core.break_label label) in
    Stack.push (Tacky.Label cont_label) stk;
    let cnd = gen_expression stk exp in
    Stack.push (Tacky.JumpIfZero (cnd, brk_label)) stk;
    gen_statement stk stmt;
    Stack.push (Tacky.Jump cont_label) stk;
    Stack.push (Tacky.Label brk_label) stk
  | C_ast.DoWhile (stmt, exp, C_ast.Identifier label) ->
    let start_label = Tacky.Identifier ("start_" ^ label) in
    let cont_label = Tacky.Identifier (Core.continue_label label) in
    let brk_label = Tacky.Identifier (Core.break_label label) in
    Stack.push (Tacky.Label start_label) stk;
    gen_statement stk stmt;
    Stack.push (Tacky.Label cont_label) stk;
    let cnd = gen_expression stk exp in
    Stack.push (Tacky.JumpIfNotZero (cnd, start_label)) stk;
    Stack.push (Tacky.Label brk_label) stk
  | C_ast.For { init; cnd; post; body; label = C_ast.Identifier label } ->
    gen_for_init stk init;
    let start_label = Tacky.Identifier ("start_" ^ label) in
    let cont_label = Tacky.Identifier (Core.continue_label label) in
    let brk_label = Tacky.Identifier (Core.break_label label) in
    Stack.push (Tacky.Label start_label) stk;
    let f cnd =
      let cnd = gen_expression stk cnd in
      Stack.push (Tacky.JumpIfZero (cnd, brk_label)) stk
    in
    let _ = Option.map f cnd in
    ();
    gen_statement stk body;
    Stack.push (Tacky.Label cont_label) stk;
    let _ = Option.map (gen_expression stk) post in
    ();
    Stack.push (Tacky.Jump start_label) stk;
    Stack.push (Tacky.Label brk_label) stk
  | C_ast.Switch _ -> assert false
  | C_ast.Null -> ()
  | C_ast.Case _ | C_ast.Default _ -> assert false

and gen_block_item stk = function
  | C_ast.S s -> gen_statement stk s
  | C_ast.D d -> gen_declaration stk d

and gen_block stk = function
  | C_ast.Block items -> List.iter (gen_block_item stk) items
;;

let gen_function_def = function
  | C_ast.Function { name; body } ->
    let stk = Stack.create () in
    gen_block stk body;
    Stack.push (Tacky.Ret (Tacky.Constant 0)) stk;
    (* The stack is effectively reversed here. *)
    let f acc a = a :: acc in
    let body = Stack.fold f [] stk in
    Tacky.Function { name = gen_identifier name; body }
;;

let gen_program = function
  | C_ast.Program f -> Tacky.Program (gen_function_def f)
;;
