open Stdint
open Common
open Ast

let dummy_var = Var (Identifier "dummy")

let make_tmp_dst vtp =
  assert (vtp <> Void);
  let c = Core.get_var_count () in
  let name = Printf.sprintf "tmp.%d" c in
  let name = Identifier name in
  SymbolMap.add_local_var name vtp;
  Var name
;;

let make_label prefix = Identifier (Core.make_unique_label prefix)

let gen_cast_value stk tgt src inner_tp =
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
;;

let rec gen_expression_tunary stk = function
  | C_ast.TUnary (tuop, prefix, lval, etp) ->
    let lval = gen_expression stk lval in
    let bop =
      match tuop with
      | C_ast.Inc -> Add
      | C_ast.Dec -> Sub
    in
    let src1 =
      match lval with
      | PlainOperand obj -> obj
      | DereferencedPointer ptr ->
        let dst = make_tmp_dst etp in
        Stack.push (Load { src_ptr = ptr; dst }) stk;
        dst
    in
    let src2 = Constant (c_type_one etp) in
    let dst =
      match prefix, bop, etp with
      | true, Add, Pointer tp ->
        Stack.push
          (AddPtr { src_ptr = src1; ind = src2; scale = size tp; dst = src1 })
          stk;
        src1
      | true, Sub, Pointer tp ->
        let tmp_dst = make_tmp_dst Long in
        Stack.push (Unary { uop = Negate; src = src2; dst = tmp_dst }) stk;
        Stack.push
          (AddPtr { src_ptr = src1; ind = tmp_dst; scale = size tp; dst = src1 })
          stk;
        src1
      | true, _, _ ->
        Stack.push (Binary { bop; src1; src2; dst = src1 }) stk;
        src1
      | false, Add, Pointer tp ->
        let dst = make_tmp_dst etp in
        Stack.push (Copy { src = src1; dst }) stk;
        Stack.push
          (AddPtr { src_ptr = src1; ind = src2; scale = size tp; dst = src1 })
          stk;
        dst
      | false, Sub, Pointer tp ->
        let dst = make_tmp_dst etp in
        Stack.push (Copy { src = src1; dst }) stk;
        let tmp_dst = make_tmp_dst Long in
        Stack.push (Unary { uop = Negate; src = src2; dst = tmp_dst }) stk;
        Stack.push
          (AddPtr { src_ptr = src1; ind = tmp_dst; scale = size tp; dst = src1 })
          stk;
        dst
      | false, _, _ ->
        let dst = make_tmp_dst etp in
        Stack.push (Copy { src = src1; dst }) stk;
        Stack.push (Binary { bop; src1; src2; dst = src1 }) stk;
        dst
    in
    (match lval with
     | PlainOperand _ -> ()
     | DereferencedPointer ptr -> Stack.push (Store { src = src1; dst_ptr = ptr }) stk);
    PlainOperand dst
  | _ -> assert false

and gen_expression_binary stk = function
  | C_ast.Binary { bop; lexp; rexp; etp } ->
    (match bop with
     | And | Or ->
       let cnd1 = gen_expression_and_convert stk lexp in
       let br_label = make_label ("other" ^ Core.binary_label) in
       let en_label = make_label ("end" ^ Core.binary_label) in
       (* Default short-circuit value. *)
       let dflt = if bop = And then 0 else 1 in
       let j1 =
         if bop = And then JumpIfZero (cnd1, br_label) else JumpIfNotZero (cnd1, br_label)
       in
       Stack.push j1 stk;
       let cnd2 = gen_expression_and_convert stk rexp in
       let j2 =
         if bop = And then JumpIfZero (cnd2, br_label) else JumpIfNotZero (cnd2, br_label)
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
     | Add ->
       let src1 = gen_expression_and_convert stk lexp in
       let src2 = gen_expression_and_convert stk rexp in
       let dst = make_tmp_dst etp in
       let b_ins =
         match C_ast.get_type lexp, C_ast.get_type rexp with
         | Pointer tp, _ -> AddPtr { src_ptr = src1; ind = src2; scale = size tp; dst }
         | _, Pointer tp -> AddPtr { src_ptr = src2; ind = src1; scale = size tp; dst }
         | _, _ -> Binary { bop; src1; src2; dst }
       in
       Stack.push b_ins stk;
       PlainOperand dst
     | Sub ->
       let src1 = gen_expression_and_convert stk lexp in
       let src2 = gen_expression_and_convert stk rexp in
       let dst = make_tmp_dst etp in
       (match C_ast.get_type lexp, C_ast.get_type rexp with
        | Pointer tp, Pointer _ ->
          let diff = make_tmp_dst Long in
          Stack.push (Binary { bop; src1; src2; dst = diff }) stk;
          let src2 = Constant (ConstLong (Int64.of_int (size tp))) in
          Stack.push (Binary { bop = Div; src1 = diff; src2; dst }) stk
        | Pointer tp, _ ->
          let tmp_dst = make_tmp_dst Long in
          Stack.push (Unary { uop = Negate; src = src2; dst = tmp_dst }) stk;
          Stack.push (AddPtr { src_ptr = src1; ind = tmp_dst; scale = size tp; dst }) stk
        | _, _ -> Stack.push (Binary { bop; src1; src2; dst }) stk);
       PlainOperand dst
     | _ ->
       let src1 = gen_expression_and_convert stk lexp in
       let src2 = gen_expression_and_convert stk rexp in
       let dst = make_tmp_dst etp in
       let b_ins = Binary { bop; src1; src2; dst } in
       Stack.push b_ins stk;
       PlainOperand dst)
  | _ -> assert false

and gen_expression stk = function
  | C_ast.Constant (c, _) -> PlainOperand (Constant c)
  | C_ast.CString (s, _) ->
    let lbl = StaticStringMap.get_const_string_label s in
    PlainOperand (Var (Identifier lbl))
  | C_ast.Var (iden, _) -> PlainOperand (Var iden)
  | C_ast.Cast { tgt; exp; _ } ->
    let src = gen_expression_and_convert stk exp in
    if tgt = Void
    then PlainOperand dummy_var
    else (
      let ret = gen_cast_value stk tgt src (C_ast.get_type exp) in
      PlainOperand ret)
  | C_ast.Unary (uop, exp, etp) ->
    let src = gen_expression_and_convert stk exp in
    let dst = make_tmp_dst etp in
    let u_ins = Unary { uop; src; dst } in
    Stack.push u_ins stk;
    PlainOperand dst
  | C_ast.TUnary _ as tu -> gen_expression_tunary stk tu
  | C_ast.Binary _ as b -> gen_expression_binary stk b
  | C_ast.CompoundAssign { bop; lexp; rexp; btp; etp } ->
    let lval = gen_expression stk lexp in
    let src2 = gen_expression_and_convert stk (C_ast.convert_to btp rexp) in
    let process src1 dst =
      match bop, etp with
      | Add, Pointer tp ->
        Stack.push (AddPtr { src_ptr = src1; ind = src2; scale = size tp; dst }) stk
      | Sub, Pointer tp ->
        let tmp_dst = make_tmp_dst Long in
        Stack.push (Unary { uop = Negate; src = src2; dst = tmp_dst }) stk;
        Stack.push (AddPtr { src_ptr = src1; ind = tmp_dst; scale = size tp; dst }) stk
      | _ -> Stack.push (Binary { bop; src1; src2; dst }) stk
    in
    (match lval with
     | PlainOperand obj ->
       let src1 = gen_cast_value stk btp obj etp in
       let dst = make_tmp_dst btp in
       process src1 dst;
       let dst = gen_cast_value stk etp dst btp in
       Stack.push (Copy { src = dst; dst = obj }) stk;
       lval
     | DereferencedPointer ptr ->
       let src1 = make_tmp_dst etp in
       Stack.push (Load { src_ptr = ptr; dst = src1 }) stk;
       let src1 = gen_cast_value stk btp src1 etp in
       let dst = make_tmp_dst btp in
       process src1 dst;
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
  | C_ast.Conditional { cnd; lhs; rhs; etp = Void } ->
    let cnd = gen_expression_and_convert stk cnd in
    let rhs_lbl = make_label ("other" ^ Core.conditional_label) in
    let en_lbl = make_label ("end" ^ Core.conditional_label) in
    Stack.push (JumpIfZero (cnd, rhs_lbl)) stk;
    let _ = gen_expression_and_convert stk lhs in
    Stack.push (Jump en_lbl) stk;
    Stack.push (Label rhs_lbl) stk;
    let _ = gen_expression_and_convert stk rhs in
    Stack.push (Label en_lbl) stk;
    PlainOperand dummy_var
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
    let dst = if etp <> Void then Some (make_tmp_dst etp) else None in
    Stack.push (FunCall { name; args; dst }) stk;
    PlainOperand (if dst = None then dummy_var else Option.get dst)
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
  | C_ast.Subscript (e1, e2, etp) ->
    let ptr, ind =
      match C_ast.get_type e1, C_ast.get_type e2 with
      | Pointer _, _ -> e1, e2
      | _, Pointer _ -> e2, e1
      | _ -> assert false
    in
    let dst = make_tmp_dst (C_ast.get_type ptr) in
    let ptr = gen_expression_and_convert stk ptr in
    let ind = gen_expression_and_convert stk ind in
    Stack.push (AddPtr { src_ptr = ptr; ind; scale = size etp; dst }) stk;
    DereferencedPointer dst
  | C_ast.SizeOf (exp, _) ->
    let res = C_ast.get_type exp |> size |> Uint64.of_int in
    PlainOperand (Constant (ConstULong res))
  | C_ast.SizeOfT (tp, _) ->
    let res = size tp |> Uint64.of_int in
    PlainOperand (Constant (ConstULong res))

and gen_expression_and_convert stk exp =
  let res = gen_expression stk exp in
  match res with
  | PlainOperand v -> v
  | DereferencedPointer ptr ->
    let dst = make_tmp_dst (C_ast.get_type exp) in
    Stack.push (Load { src_ptr = ptr; dst }) stk;
    dst
;;

let rec gen_compound_initializer stk dst offset = function
  | C_ast.SingleInit (CString (s, CArray (_, sz)), _) ->
    let diff = sz - String.length s in
    let fill = Char.chr 0 |> Bytes.make diff in
    let bytes = Bytes.cat (Bytes.of_string s) fill in
    let rec loop i =
      let src, nxt =
        if i + 8 <= sz
        then (
          let v = Bytes.get_int64_le bytes i |> Uint64.of_int64 in
          Constant (ConstULong v), i + 8)
        else if i + 4 <= sz
        then (
          let v = Bytes.get_int32_le bytes i |> Uint32.of_int32 in
          Constant (ConstUInt v), i + 4)
        else if i + 1 <= sz
        then (
          let v = Bytes.get_uint8 bytes i |> Int32.of_int in
          Constant (ConstUChar v), i + 1)
        else Constant (ConstUChar 0l), 0
      in
      if nxt > 0
      then (
        Stack.push (CopyToOffset { src; dst; offset = offset + i }) stk;
        loop nxt)
    in
    loop 0
  | C_ast.SingleInit (exp, _) ->
    let src = gen_expression_and_convert stk exp in
    Stack.push (CopyToOffset { src; dst; offset }) stk
  | C_ast.CompoundInit (exp_list, tp) ->
    let atp =
      match tp with
      | CArray (tp, _) -> tp
      | _ -> assert false
    in
    List.iteri
      (fun ind exp -> gen_compound_initializer stk dst (offset + (ind * size atp)) exp)
      exp_list
;;

let gen_variable_decl stk = function
  | C_ast.{ name; init = Some c_init; vtp; storage = None } ->
    let dst = Var name in
    (match vtp, c_init with
     | CArray _, _ -> gen_compound_initializer stk dst 0 c_init
     | _, C_ast.SingleInit (exp, _) ->
       let src = gen_expression_and_convert stk exp in
       Stack.push (Copy { src; dst }) stk
     | _ -> assert false)
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
    let exp_val = Option.map (gen_expression_and_convert stk) exp in
    Stack.push (Ret exp_val) stk
  | C_ast.Expression exp ->
    let _ = gen_expression_and_convert stk exp in
    ()
  | C_ast.If { cnd; thn; els } ->
    let cnd = gen_expression_and_convert stk cnd in
    let els_lbl =
      if els != None then make_label (Core.else_if_label ()) else Identifier "not_used"
    in
    let en_lbl = make_label (Core.end_if_label ()) in
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
    let start_label = Identifier (Core.start_loop_label label) in
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
    let start_label = Identifier (Core.start_loop_label label) in
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
      | FunType { ret = Void; _ } -> None
      | FunType { ret; _ } -> Some (Constant (c_type_zero ret))
      | _ -> assert false
    in
    Stack.push (Ret ret_val) stk;
    (* The stack is effectively reversed here. *)
    let f acc a = a :: acc in
    let body = Stack.fold f [] stk in
    let tacky_f =
      Function { name; global = SymbolMap.is_global_fun name; params; body }
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
  Hashtbl.fold
    (fun iden SymbolMap.{ tp; attrs } acc ->
       match attrs with
       | StaticAttr { init; global } ->
         let name = Identifier iden in
         (match init with
          | Initial i -> StaticVar { name; global; tp; init_list = i } :: acc
          | Tentative ->
            let init_list = [ init_zero tp ] in
            StaticVar { name; global; tp; init_list } :: acc
          | NoInitial -> acc)
       | ConstantAttr init -> StaticConstant { name = Identifier iden; tp; init } :: acc
       | _ -> acc)
    SymbolMap.symbol_map
    acc
;;

let gen_program = function
  | C_ast.Program dns ->
    let top_lvls = List.filter_map gen_declaration dns in
    Program (convert_symbols_to_tacky top_lvls)
;;
