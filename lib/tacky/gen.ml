open Stdint
open Common
open Ast

let gen_uop = function
  | C_ast.Complement -> Complement
  | C_ast.Negate -> Negate
  | C_ast.Not -> Not
;;

let gen_bop = function
  | C_ast.Add -> Add
  | C_ast.Sub -> Sub
  | C_ast.Mul -> Mul
  | C_ast.Div -> Div
  | C_ast.Rem -> Rem
  | C_ast.BAnd -> And
  | C_ast.BOr -> Or
  | C_ast.Xor -> Xor
  | C_ast.Lsft -> Lsft
  | C_ast.Rsft -> Rsft
  | C_ast.Equal -> Equal
  | C_ast.NEqual -> NEqual
  | C_ast.Less -> Less
  | C_ast.LEqual -> LEqual
  | C_ast.GEqual -> GEqual
  | C_ast.Greater -> Greater
  (* And and Or handled differently *)
  | C_ast.And | C_ast.Or -> assert false
;;

let make_tmp_dst vtp =
  let c = Core.get_var_count () in
  let name = Printf.sprintf "tmp.%d" c in
  let name = Identifier name in
  Symbol_map.add_local_var name vtp;
  Var name
;;

let make_label prefix = Identifier (Core.make_unique_label prefix)

let gen_cast_value stk tgt src inner_tp =
  let ret =
    if tgt = inner_tp
    then src
    else (
      let dst = make_tmp_dst tgt in
      if inner_tp = Double
      then
        if signed_c_type tgt
        then Stack.push (DoubleToInt { src; dst }) stk
        else Stack.push (DoubleToUInt { src; dst }) stk
      else if tgt = Double
      then
        if signed_c_type inner_tp
        then Stack.push (IntToDouble { src; dst }) stk
        else Stack.push (UIntToDouble { src; dst }) stk
      else if size tgt = size inner_tp
      then Stack.push (Copy { src; dst }) stk
      else if size tgt < size inner_tp
      then Stack.push (Truncate { src; dst }) stk
      else if signed_c_type inner_tp
      then Stack.push (SignExtend { src; dst }) stk
      else Stack.push (ZeroExtend { src; dst }) stk;
      dst)
  in
  ret
;;

let rec gen_expression stk = function
  | C_ast.Constant (c, _) -> PlainOperand (Constant c)
  | C_ast.Var (iden, _) -> PlainOperand (Var iden)
  | C_ast.Cast { tgt; exp; _ } ->
    let src = gen_expression_and_convert stk exp in
    let ret = gen_cast_value stk tgt src (C_ast.get_type exp) in
    PlainOperand ret
  | C_ast.Unary (uop, exp, etp) ->
    let uop = gen_uop uop in
    let src = gen_expression_and_convert stk exp in
    let dst = make_tmp_dst etp in
    let u_ins = Unary { uop; src; dst } in
    Stack.push u_ins stk;
    PlainOperand dst
  | C_ast.TUnary (tuop, prefix, lval, etp) ->
    let lval = gen_expression stk lval in
    let bop =
      match tuop with
      | C_ast.Inc -> Add
      | C_ast.Dec -> Sub
    in
    let const_one = c_type_one etp in
    let src =
      match lval with
      | PlainOperand obj -> obj
      | DereferencedPointer ptr ->
        let dst = make_tmp_dst etp in
        Stack.push (Load { src_ptr = ptr; dst }) stk;
        dst
    in
    let dst =
      match prefix with
      | true ->
        Stack.push (Binary { bop; src1 = src; src2 = Constant const_one; dst = src }) stk;
        src
      | false ->
        let dst = make_tmp_dst etp in
        Stack.push (Copy { src; dst }) stk;
        Stack.push (Binary { bop; src1 = src; src2 = Constant const_one; dst = src }) stk;
        dst
    in
    (match lval with
     | PlainOperand _ -> ()
     | DereferencedPointer ptr -> Stack.push (Store { src; dst_ptr = ptr }) stk);
    PlainOperand dst
  | C_ast.Binary { bop; lexp; rexp; etp } ->
    (match bop with
     | C_ast.And | C_ast.Or ->
       let cnd1 = gen_expression_and_convert stk lexp in
       let br_label = make_label ("other" ^ Core.binary_label) in
       let en_label = make_label ("end" ^ Core.binary_label) in
       (* Default short-circuit value. *)
       let dflt = if bop = C_ast.And then 0 else 1 in
       let j1 =
         if bop = C_ast.And
         then JumpIfZero (cnd1, br_label)
         else JumpIfNotZero (cnd1, br_label)
       in
       Stack.push j1 stk;
       let cnd2 = gen_expression_and_convert stk rexp in
       let j2 =
         if bop = C_ast.And
         then JumpIfZero (cnd2, br_label)
         else JumpIfNotZero (cnd2, br_label)
       in
       Stack.push j2 stk;
       let dst = make_tmp_dst Int in
       Stack.push
         (Copy { src = Constant (ConstInt (Int32.of_int (1 lxor dflt))); dst })
         stk;
       Stack.push (Jump en_label) stk;
       Stack.push (Label br_label) stk;
       Stack.push (Copy { src = Constant (ConstInt (Int32.of_int dflt)); dst }) stk;
       Stack.push (Label en_label) stk;
       PlainOperand dst
     | _ ->
       let bop = gen_bop bop in
       let src1 = gen_expression_and_convert stk lexp in
       let src2 = gen_expression_and_convert stk rexp in
       let dst = make_tmp_dst etp in
       let b_ins = Binary { bop; src1; src2; dst } in
       Stack.push b_ins stk;
       PlainOperand dst)
  | C_ast.CompoundAssign { bop; lexp; rexp; btp; etp } ->
    let bop = gen_bop bop in
    let lval = gen_expression stk lexp in
    let src2 = gen_expression_and_convert stk (C_ast.convert_to btp rexp) in
    (match lval with
     | PlainOperand obj ->
       let src1 = gen_cast_value stk btp obj etp in
       let dst = make_tmp_dst btp in
       Stack.push (Binary { bop; src1; src2; dst }) stk;
       let dst = gen_cast_value stk etp dst btp in
       Stack.push (Copy { src = dst; dst = obj }) stk;
       lval
     | DereferencedPointer ptr ->
       let src1 = make_tmp_dst etp in
       Stack.push (Load { src_ptr = ptr; dst = src1 }) stk;
       let src1 = gen_cast_value stk btp src1 etp in
       let dst = make_tmp_dst btp in
       Stack.push (Binary { bop; src1; src2; dst }) stk;
       let dst = gen_cast_value stk etp dst btp in
       Stack.push (Store { src = dst; dst_ptr = ptr }) stk;
       PlainOperand dst)
  | C_ast.Assignment { lval; rval; _ } ->
    let lval = gen_expression stk lval in
    let rval = gen_expression_and_convert stk rval in
    (match lval with
     | PlainOperand obj ->
       Stack.push (Copy { src = rval; dst = obj }) stk;
       lval
     | DereferencedPointer ptr ->
       Stack.push (Store { src = rval; dst_ptr = ptr }) stk;
       PlainOperand rval)
  | C_ast.Conditional { cnd; lhs; rhs; etp } ->
    let cnd = gen_expression_and_convert stk cnd in
    let rhs_lbl = make_label ("other" ^ Core.conditional_label) in
    let en_lbl = make_label ("end" ^ Core.conditional_label) in
    let dst = make_tmp_dst etp in
    Stack.push (JumpIfZero (cnd, rhs_lbl)) stk;
    let src = gen_expression_and_convert stk lhs in
    Stack.push (Copy { src; dst }) stk;
    Stack.push (Jump en_lbl) stk;
    Stack.push (Label rhs_lbl) stk;
    let src = gen_expression_and_convert stk rhs in
    Stack.push (Copy { src; dst }) stk;
    Stack.push (Label en_lbl) stk;
    PlainOperand dst
  | C_ast.FunctionCall (name, args, etp) ->
    let args = List.map (gen_expression_and_convert stk) args in
    let dst = make_tmp_dst etp in
    Stack.push (FunCall { name; args; dst }) stk;
    PlainOperand dst
  | C_ast.Dereference (exp, _) ->
    let res = gen_expression_and_convert stk exp in
    DereferencedPointer res
  | C_ast.AddrOf (exp, etp) ->
    let v = gen_expression stk exp in
    (match v with
     | PlainOperand obj ->
       let dst = make_tmp_dst etp in
       Stack.push (GetAddr { src = obj; dst }) stk;
       PlainOperand dst
     | DereferencedPointer ptr -> PlainOperand ptr)

and gen_expression_and_convert stk exp =
  let res = gen_expression stk exp in
  match res with
  | PlainOperand v -> v
  | DereferencedPointer ptr ->
    let dst = make_tmp_dst (C_ast.get_type exp) in
    Stack.push (Load { src_ptr = ptr; dst }) stk;
    dst
;;

let gen_variable_decl stk = function
  | C_ast.{ name; init = Some exp; storage = None; _ } ->
    let exp_val = gen_expression_and_convert stk exp in
    Stack.push (Copy { src = exp_val; dst = Var name }) stk
  | _ -> ()
;;

let gen_for_init stk = function
  | C_ast.InitDecl d -> gen_variable_decl stk d
  | C_ast.InitExp e ->
    (match e with
     | Some e ->
       let _ = gen_expression_and_convert stk e in
       ()
     | None -> ())
;;

let rec gen_statement stk = function
  | C_ast.Return exp ->
    let exp_val = gen_expression_and_convert stk exp in
    Stack.push (Ret exp_val) stk
  | C_ast.Expression exp ->
    let _ = gen_expression_and_convert stk exp in
    ()
  | C_ast.If { cnd; thn; els } ->
    let cnd = gen_expression_and_convert stk cnd in
    let els_lbl =
      if els != None then make_label ("else@" ^ Core.if_label) else Identifier "not_used"
    in
    let en_lbl = make_label ("end@" ^ Core.if_label) in
    let to_go = if els != None then els_lbl else en_lbl in
    Stack.push (JumpIfZero (cnd, to_go)) stk;
    let () = gen_statement stk thn in
    if els != None
    then (
      Stack.push (Jump en_lbl) stk;
      Stack.push (Label els_lbl) stk;
      let _ = Option.map (gen_statement stk) els in
      ())
    else ();
    Stack.push (Label en_lbl) stk
  | C_ast.Goto label -> Stack.push (Jump label) stk
  | C_ast.Label (label, stmt) ->
    Stack.push (Label label) stk;
    gen_statement stk stmt
  | C_ast.Compound block -> gen_block stk block
  | C_ast.Break (Identifier label) ->
    let label = Identifier (Core.break_label label) in
    Stack.push (Jump label) stk;
    ()
  | C_ast.Continue (Identifier label) ->
    let label = Identifier (Core.continue_label label) in
    Stack.push (Jump label) stk;
    ()
  | C_ast.While (exp, stmt, Identifier label) ->
    let cont_label = Identifier (Core.continue_label label) in
    let brk_label = Identifier (Core.break_label label) in
    Stack.push (Label cont_label) stk;
    let cnd = gen_expression_and_convert stk exp in
    Stack.push (JumpIfZero (cnd, brk_label)) stk;
    gen_statement stk stmt;
    Stack.push (Jump cont_label) stk;
    Stack.push (Label brk_label) stk
  | C_ast.DoWhile (stmt, exp, Identifier label) ->
    let start_label = Identifier ("start@" ^ label) in
    let cont_label = Identifier (Core.continue_label label) in
    let brk_label = Identifier (Core.break_label label) in
    Stack.push (Label start_label) stk;
    gen_statement stk stmt;
    Stack.push (Label cont_label) stk;
    let cnd = gen_expression_and_convert stk exp in
    Stack.push (JumpIfNotZero (cnd, start_label)) stk;
    Stack.push (Label brk_label) stk
  | C_ast.For { init; cnd; post; body; label = Identifier label } ->
    gen_for_init stk init;
    let start_label = Identifier ("start@" ^ label) in
    let cont_label = Identifier (Core.continue_label label) in
    let brk_label = Identifier (Core.break_label label) in
    Stack.push (Label start_label) stk;
    let f cnd =
      let cnd = gen_expression_and_convert stk cnd in
      Stack.push (JumpIfZero (cnd, brk_label)) stk
    in
    let _ = Option.map f cnd in
    ();
    gen_statement stk body;
    Stack.push (Label cont_label) stk;
    let _ = Option.map (gen_expression_and_convert stk) post in
    ();
    Stack.push (Jump start_label) stk;
    Stack.push (Label brk_label) stk
  | C_ast.Switch { cnd; body; cases; default; label = Identifier label } ->
    let src1 = gen_expression_and_convert stk cnd in
    let dst = make_tmp_dst Int in
    let calc_jmp ind cn =
      let jmp_lbl = Identifier (Core.case_label ind label) in
      let jmp_ins = Binary { bop = Equal; src1; src2 = Constant cn; dst } in
      Stack.push jmp_ins stk;
      Stack.push (JumpIfNotZero (dst, jmp_lbl)) stk
    in
    List.iteri calc_jmp cases;
    let en_lbl = Identifier (Core.break_label label) in
    if default
    then (
      let jmp_ins = Identifier (Core.default_label label) in
      Stack.push (Jump jmp_ins) stk)
    else Stack.push (Jump en_lbl) stk;
    gen_statement stk body;
    Stack.push (Label en_lbl) stk
  | C_ast.Null -> ()
  | C_ast.Case _ | C_ast.Default _ -> assert false

and gen_block_item stk = function
  | C_ast.S s -> gen_statement stk s
  | C_ast.D d ->
    (match d with
     | VarDecl v -> gen_variable_decl stk v
     | FunDecl f -> assert (f.body = None))

and gen_block stk = function
  | C_ast.Block items -> List.iter (gen_block_item stk) items
;;

let gen_function_decl = function
  | C_ast.{ name; params; body = Some body; ftp; _ } ->
    let stk = Stack.create () in
    gen_block stk body;
    let ret_val =
      match ftp with
      | FunType { ret; _ } -> c_type_zero ret
      | _ -> assert false
    in
    Stack.push (Ret (Constant ret_val)) stk;
    (* The stack is effectively reversed here. *)
    let f acc a = a :: acc in
    let body = Stack.fold f [] stk in
    let tacky_f =
      Function { name; global = Symbol_map.is_global_fun name; params; body }
    in
    Some tacky_f
  (* Do nothing for empty function bodies. *)
  | _ -> None
;;

let gen_declaration = function
  | C_ast.VarDecl _ -> None
  | C_ast.FunDecl f -> gen_function_decl f
;;

let convert_symbols_to_tacky acc =
  Symbol_map.fold (fun name global tp init -> StaticVar { name; global; tp; init }) acc
;;

let gen_program = function
  | C_ast.Program dns ->
    let top_lvls = List.filter_map gen_declaration dns in
    Program (convert_symbols_to_tacky top_lvls)
;;
