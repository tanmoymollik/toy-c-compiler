open Stdint
open Common
open AsmUtils
open Ast

(* Maps static double constants to label. *)
let static_consts : (int64 * int, string) Hashtbl.t = Hashtbl.create 10
let imm_zr = Imm 0I
let imm_one = Imm 1I

let gen_uop : Common.unary_op -> Ast.unary_op = function
  | Complement -> Not
  | Negate -> Neg
  (* Handled differently. *)
  | Not -> assert false
;;

let gen_bop : Common.binary_op -> Ast.binary_op = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Imul
  | BAnd -> And
  | BOr -> Or
  | Xor -> Xor
  | Lsft -> Sal
  | Rsft -> Sar
  (* Handled differently. *)
  | And | Or | Div | Rem | Equal | NEqual | Less | LEqual | Greater | GEqual ->
    assert false
;;

let gen_cond_code signed = function
  | Equal -> E
  | NEqual -> NE
  | Less -> if signed then L else B
  | LEqual -> if signed then LE else BE
  | Greater -> if signed then G else A
  | GEqual -> if signed then GE else AE
  (* Not conditional ops. *)
  | _ -> assert false
;;

let get_double_const cd alignment =
  let ci = Stdlib.Int64.bits_of_float cd in
  let const_label =
    match Hashtbl.find_opt static_consts (ci, alignment) with
    | Some lbl -> lbl
    | None ->
      let lbl = Core.make_unique_label Core.static_const_label in
      Hashtbl.replace static_consts (ci, alignment) lbl;
      lbl
  in
  let var = Identifier const_label in
  AsmSymbolMap.add_obj_info const_label Double true false;
  Pseudo var
;;

let gen_const = function
  | ConstDouble d -> get_double_const d 8
  | x -> Imm (TypeConverter.convert_to_ulong x)
;;

let gen_value = function
  | Tacky.Ast.Constant c -> gen_const c
  | Tacky.Ast.Var name ->
    (match AsmSymbolMap.get_var_type name with
     | ByteArray _ -> PseudoMem (name, 0)
     | _ -> Pseudo name)
;;

let signed = function
  | Tacky.Ast.Constant c -> signed_const c
  | Tacky.Ast.Var name -> AsmSymbolMap.is_signed_var name
;;

let gen_ins_div = function
  | Tacky.Ast.Binary { bop; src1; src2; dst } ->
    let op_tp = get_asm_type_for_val dst in
    let signed = signed dst in
    (match op_tp, bop with
     | AsmDouble, Div ->
       let dst = gen_value dst in
       [ Mov { src = gen_value src1; dst; tp = op_tp }
       ; Binary { bop = DivDouble; src = gen_value src2; dst; tp = op_tp }
       ]
     | AsmDouble, Rem -> assert false
     | _ ->
       let ex_ins =
         if signed then Cdq op_tp else Mov { src = imm_zr; dst = Reg Dx; tp = op_tp }
       in
       let tmp_dst =
         match bop with
         | Div -> Reg Ax
         | Rem -> Reg Dx
         | _ -> assert false
       in
       let div_ins =
         if signed then Idiv (gen_value src2, op_tp) else Div (gen_value src2, op_tp)
       in
       [ Mov { src = gen_value src1; dst = Reg Ax; tp = op_tp }
       ; ex_ins
       ; div_ins
       ; Mov { src = tmp_dst; dst = gen_value dst; tp = op_tp }
       ])
  | _ -> assert false
;;

let gen_ins_uop = function
  (* dst is always Tacky.Ast.Var *)
  | Tacky.Ast.Unary { uop; src; dst } ->
    (match uop with
     | Complement ->
       let tp = get_asm_type_for_val dst in
       [ Mov { src = gen_value src; dst = gen_value dst; tp }
       ; Unary (gen_uop uop, gen_value dst, tp)
       ]
     | Negate ->
       let tp = get_asm_type_for_val dst in
       (match tp with
        | AsmDouble ->
          let neg_zr = get_double_const (-0.0) 16 in
          let dst = gen_value dst in
          [ Mov { src = gen_value src; dst; tp }
          ; Binary { bop = Xor; src = neg_zr; dst; tp }
          ]
        | _ ->
          [ Mov { src = gen_value src; dst = gen_value dst; tp }
          ; Unary (gen_uop uop, gen_value dst, tp)
          ])
     | Not ->
       let src_tp = get_asm_type_for_val src in
       let dst_tp = get_asm_type_for_val dst in
       let dst = gen_value dst in
       (match src_tp with
        | AsmDouble ->
          let zr_reg = Reg Xmm0 in
          let nan_cmp_end = Identifier (Core.make_unique_label Core.nan_cmp_end_label) in
          [ Binary { bop = Xor; src = zr_reg; dst = zr_reg; tp = src_tp }
          ; Cmp { lhs = gen_value src; rhs = zr_reg; tp = src_tp }
          ; Mov { src = imm_zr; dst; tp = dst_tp }
          ; JmpP nan_cmp_end
          ; SetC (E, dst)
          ; Label nan_cmp_end
          ]
        | _ ->
          [ Cmp { lhs = gen_value src; rhs = imm_zr; tp = src_tp }
          ; Mov { src = imm_zr; dst; tp = dst_tp }
          ; SetC (E, dst)
          ]))
  | _ -> assert false
;;

let gen_ins_bop = function
  | Tacky.Ast.Binary { bop; src1; src2; dst } as ret ->
    (match bop with
     | Div | Rem -> gen_ins_div ret
     | Equal | NEqual | Less | LEqual | Greater | GEqual ->
       let op_tp = get_asm_type_for_val src1 in
       let dst_tp = get_asm_type_for_val dst in
       assert (dst_tp = DWord);
       let dst = gen_value dst in
       (match op_tp, bop with
        | AsmDouble, NEqual ->
          let nan_cmp = Identifier (Core.make_unique_label Core.nan_cmp_label) in
          let nan_cmp_end = Identifier (Core.make_unique_label Core.nan_cmp_end_label) in
          [ Cmp { lhs = gen_value src1; rhs = gen_value src2; tp = op_tp }
          ; Mov { src = imm_zr; dst; tp = dst_tp }
          ; JmpP nan_cmp
          ; SetC (gen_cond_code false bop, dst)
          ; Jmp nan_cmp_end
          ; Label nan_cmp
          ; Mov { src = imm_one; dst; tp = dst_tp }
          ; Label nan_cmp_end
          ]
        | AsmDouble, _ ->
          let nan_cmp_end = Identifier (Core.make_unique_label Core.nan_cmp_end_label) in
          [ Cmp { lhs = gen_value src1; rhs = gen_value src2; tp = op_tp }
          ; Mov { src = imm_zr; dst; tp = dst_tp }
          ; JmpP nan_cmp_end
          ; SetC (gen_cond_code false bop, dst)
          ; Label nan_cmp_end
          ]
        | _ ->
          let signed = signed src1 in
          [ Cmp { lhs = gen_value src1; rhs = gen_value src2; tp = op_tp }
          ; Mov { src = imm_zr; dst; tp = dst_tp }
          ; SetC (gen_cond_code signed bop, dst)
          ])
     | Add | Sub | Mul | BAnd | BOr | Xor ->
       let op_tp = get_asm_type_for_val dst in
       let dst = gen_value dst in
       [ Mov { src = gen_value src1; dst; tp = op_tp }
       ; Binary { bop = gen_bop bop; src = gen_value src2; dst; tp = op_tp }
       ]
     | Lsft | Rsft ->
       let op_tp = get_asm_type_for_val dst in
       let signed = signed dst in
       let src2 = gen_value src2 in
       let tmp_src2 = Reg Cx in
       let dst = gen_value dst in
       let bop = gen_bop bop in
       let bop = if (not signed) && bop = Sar then Shr else bop in
       [ Mov { src = gen_value src1; dst; tp = op_tp }
       ; Mov { src = src2; dst = tmp_src2; tp = op_tp }
       ; Binary { bop; src = tmp_src2; dst; tp = op_tp }
       ]
     | And | Or -> assert false)
  | _ -> assert false
;;

let classify_params params =
  let int_reg_args = Stack.create () in
  let double_reg_args = Stack.create () in
  let stack_args = Stack.create () in
  let loop_iter param =
    let oprnd = gen_value param in
    let tp = get_asm_type_for_val param in
    let typed_oprnd = oprnd, tp in
    if tp = AsmDouble
    then
      if Stack.length double_reg_args < 8
      then Stack.push oprnd double_reg_args
      else Stack.push typed_oprnd stack_args
    else if Stack.length int_reg_args < 6
    then Stack.push typed_oprnd int_reg_args
    else Stack.push typed_oprnd stack_args
  in
  List.iter loop_iter params;
  let reverser stk = Stack.fold (fun acc v -> v :: acc) [] stk in
  reverser int_reg_args, reverser double_reg_args, reverser stack_args
;;

let gen_instruction = function
  | Tacky.Ast.Ret v ->
    let tp = get_asm_type_for_val v in
    let dst = if tp = AsmDouble then Reg Xmm0 else Reg Ax in
    [ Mov { src = gen_value v; dst; tp }; Ret ]
  | Tacky.Ast.Unary _ as ret -> gen_ins_uop ret
  (* dst is always Tacky.Ast.Var *)
  | Tacky.Ast.Binary _ as ret -> gen_ins_bop ret
  | Tacky.Ast.Copy { src; dst } ->
    let tp = get_asm_type_for_val dst in
    [ Mov { src = gen_value src; dst = gen_value dst; tp } ]
  | Tacky.Ast.Jump iden -> [ Jmp iden ]
  | Tacky.Ast.JumpIfZero (cnd, tgt) ->
    let tp = get_asm_type_for_val cnd in
    (match tp with
     | AsmDouble ->
       let zr_reg = Reg Xmm0 in
       let nan_cmp_end = Identifier (Core.make_unique_label Core.nan_cmp_end_label) in
       [ Binary { bop = Xor; src = zr_reg; dst = zr_reg; tp }
       ; Cmp { lhs = gen_value cnd; rhs = zr_reg; tp }
       ; JmpP nan_cmp_end
       ; JmpC (E, tgt)
       ; Label nan_cmp_end
       ]
     | _ -> [ Cmp { lhs = gen_value cnd; rhs = imm_zr; tp }; JmpC (E, tgt) ])
  | Tacky.Ast.JumpIfNotZero (cnd, tgt) ->
    let tp = get_asm_type_for_val cnd in
    (match tp with
     | AsmDouble ->
       let zr_reg = Reg Xmm0 in
       [ Binary { bop = Xor; src = zr_reg; dst = zr_reg; tp }
       ; Cmp { lhs = gen_value cnd; rhs = zr_reg; tp }
       ; JmpP tgt
       ; JmpC (NE, tgt)
       ]
     | _ -> [ Cmp { lhs = gen_value cnd; rhs = imm_zr; tp }; JmpC (NE, tgt) ])
  | Tacky.Ast.Label iden -> [ Label iden ]
  | Tacky.Ast.FunCall { name; args; dst } ->
    let int_args, double_args, stk_args = classify_params args in
    let take_n n lst = List.filteri (fun i _ -> i < n) lst in
    FuncInfo.save_func_params
      name
      (take_n (List.length int_args) int_regs)
      (take_n (List.length double_args) double_regs);
    let stack_padding = if List.length stk_args mod 2 = 1 then 8 else 0 in
    let stack_alloc = if stack_padding <> 0 then [ alloc_stack_ins 8 ] else [] in
    let f reg_list ind (operand, tp) =
      Mov { src = operand; dst = Reg (List.nth reg_list ind); tp }
    in
    let int_reg_ins = List.mapi (f int_regs) int_args in
    let double_reg_ins =
      List.mapi (fun ind operand -> f double_regs ind (operand, AsmDouble)) double_args
    in
    let f (operand, tp) =
      match tp with
      | QWord | AsmDouble -> [ Push operand ]
      | _ ->
        (match operand with
         | Imm _ | Reg _ -> [ Push operand ]
         | _ ->
           let dst = Reg Ax in
           [ Mov { src = operand; dst; tp }; Push dst ])
    in
    let stk_ins = List.concat_map f (List.rev stk_args) in
    let fun_call = [ Call name ] in
    let bytes_to_remove = (8 * List.length stk_args) + stack_padding in
    let stack_dealloc =
      if bytes_to_remove <> 0 then [ dealloc_stack_ins bytes_to_remove ] else []
    in
    let tp = AsmSymbolMap.get_fun_ret_type name in
    let dst = gen_value dst in
    let src = if tp = AsmDouble then Reg Xmm0 else Reg Ax in
    let ret_ins = [ Mov { src; dst; tp } ] in
    stack_alloc
    @ int_reg_ins
    @ double_reg_ins
    @ stk_ins
    @ fun_call
    @ stack_dealloc
    @ ret_ins
  | Tacky.Ast.SignExtend { src; dst } ->
    let src = gen_value src in
    let dst = gen_value dst in
    [ Movsx { src; dst } ]
  | Tacky.Ast.Truncate { src; dst } ->
    let src = gen_value src in
    let dst = gen_value dst in
    [ Mov { src; dst; tp = DWord } ]
  | Tacky.Ast.ZeroExtend { src; dst } ->
    let src = gen_value src in
    let dst = gen_value dst in
    [ MovZeroExtend { src; dst } ]
  | Tacky.Ast.IntToDouble { src; dst } ->
    let src_tp = get_asm_type_for_val src in
    let src = gen_value src in
    let dst = gen_value dst in
    [ Cvtsi2sd { src; dst; src_tp } ]
  | Tacky.Ast.DoubleToInt { src; dst } ->
    let dst_tp = get_asm_type_for_val dst in
    let src = gen_value src in
    let dst = gen_value dst in
    [ Cvttsd2si { src; dst; dst_tp } ]
  | Tacky.Ast.UIntToDouble { src; dst } ->
    let src_tp = get_asm_type_for_val src in
    let src = gen_value src in
    let dst = gen_value dst in
    let label1 = Core.make_unique_label "UIntToDouble" in
    let label2 = Core.make_unique_label "UIntToDoubleEnd" in
    (match src_tp with
     | DWord ->
       let tmp_src = Reg Ax in
       [ MovZeroExtend { src; dst = tmp_src }
       ; Cvtsi2sd { src = tmp_src; dst; src_tp = QWord }
       ]
     | QWord ->
       let reg1 = Reg Ax in
       let reg2 = Reg Dx in
       [ Cmp { lhs = src; rhs = imm_zr; tp = QWord }
       ; JmpC (L, Identifier label1)
       ; Cvtsi2sd { src; dst; src_tp = QWord }
       ; Jmp (Identifier label2)
       ; Label (Identifier label1)
       ; Mov { src; dst = reg1; tp = QWord }
       ; Mov { src = reg1; dst = reg2; tp = QWord }
       ; Binary { bop = Shr; src = imm_one; dst = reg2; tp = QWord }
       ; Binary { bop = And; src = imm_one; dst = reg1; tp = QWord }
       ; Binary { bop = Or; src = reg1; dst = reg2; tp = QWord }
       ; Cvtsi2sd { src = reg2; dst; src_tp = QWord }
       ; Binary { bop = Add; src = dst; dst; tp = AsmDouble }
       ; Label (Identifier label2)
       ]
     | _ -> assert false)
  | Tacky.Ast.DoubleToUInt { src; dst } ->
    let dst_tp = get_asm_type_for_val dst in
    let src = gen_value src in
    let dst = gen_value dst in
    (match dst_tp with
     | DWord ->
       let tmp_dst = Reg Ax in
       [ Cvttsd2si { src; dst = tmp_dst; dst_tp = QWord }
       ; Mov { src = tmp_dst; dst; tp = DWord }
       ]
     | QWord ->
       let regX = Reg Xmm0 in
       let regR = Reg Ax in
       let upper_bound = get_double_const 9223372036854775808.0 8 in
       let upper_bound_int = Uint64.of_int64 9223372036854775808L in
       let label1 = Core.make_unique_label "DoubleToUInt" in
       let label2 = Core.make_unique_label "DoubleToUIntEnd" in
       [ Cmp { lhs = src; rhs = upper_bound; tp = AsmDouble }
       ; JmpC (AE, Identifier label1)
       ; Cvttsd2si { src; dst; dst_tp = QWord }
       ; Jmp (Identifier label2)
       ; Label (Identifier label1)
       ; Mov { src; dst = regX; tp = AsmDouble }
       ; Binary { bop = Sub; src = upper_bound; dst = regX; tp = AsmDouble }
       ; Cvttsd2si { src = regX; dst; dst_tp = QWord }
       ; Mov { src = Imm upper_bound_int; dst = regR; tp = QWord }
       ; Binary { bop = Add; src = regR; dst; tp = QWord }
       ; Label (Identifier label2)
       ]
     | _ -> assert false)
  | Tacky.Ast.GetAddr { src; dst } -> [ Lea { src = gen_value src; dst = gen_value dst } ]
  | Tacky.Ast.Load { src_ptr; dst } ->
    [ Mov { src = gen_value src_ptr; dst = Reg Ax; tp = QWord }
    ; Mov { src = Memory (Ax, 0); dst = gen_value dst; tp = get_asm_type_for_val dst }
    ]
  | Tacky.Ast.Store { src; dst_ptr } ->
    [ Mov { src = gen_value dst_ptr; dst = Reg Ax; tp = QWord }
    ; Mov { src = gen_value src; dst = Memory (Ax, 0); tp = get_asm_type_for_val src }
    ]
  | Tacky.Ast.AddPtr { src_ptr; ind; scale; dst } ->
    let dst = gen_value dst in
    (match ind with
     | Tacky.Ast.Constant c ->
       let ind = Int64.to_int (TypeConverter.convert_to_long c) in
       [ Mov { src = gen_value src_ptr; dst = Reg Ax; tp = QWord }
       ; Lea { src = Memory (Ax, -(ind * scale)); dst }
       ]
     | _ ->
       (match scale with
        | 1 | 2 | 4 | 8 ->
          [ Mov { src = gen_value src_ptr; dst = Reg Ax; tp = QWord }
          ; Mov { src = gen_value ind; dst = Reg Dx; tp = QWord }
          ; Lea { src = Indexed { base = Ax; ind = Dx; scale }; dst }
          ]
        | _ ->
          [ Mov { src = gen_value src_ptr; dst = Reg Ax; tp = QWord }
          ; Mov { src = gen_value ind; dst = Reg Dx; tp = QWord }
          ; Binary
              { bop = Imul; src = Imm (Uint64.of_int scale); dst = Reg Dx; tp = QWord }
          ; Lea { src = Indexed { base = Ax; ind = Dx; scale = 1 }; dst }
          ]))
  | Tacky.Ast.CopyToOffset { src; dst; offset } ->
    let dst =
      match dst with
      | Var name -> PseudoMem (name, offset)
      | _ -> assert false
    in
    [ Mov { src = gen_value src; dst; tp = get_asm_type_for_val src } ]
;;

let set_up_params params =
  let int_reg_params, double_reg_params, stk_params = classify_params params in
  let f reg_list ind (oprnd, tp) =
    Mov { src = Reg (List.nth reg_list ind); dst = oprnd; tp }
  in
  let int_reg_ins = List.mapi (f int_regs) int_reg_params in
  let double_reg_ins =
    List.mapi (fun ind oprnd -> f double_regs ind (oprnd, AsmDouble)) double_reg_params
  in
  let stk_ins =
    List.mapi
      (fun ind (oprnd, tp) ->
         let addr = -(16 + (8 * ind)) in
         let src = Memory (Bp, addr) in
         Mov { src; dst = oprnd; tp })
      stk_params
  in
  int_reg_ins @ double_reg_ins @ stk_ins
;;

let gen_top_level = function
  | Tacky.Ast.Function { name; global; params; body } ->
    let params = List.map (fun param -> Tacky.Ast.Var param) params in
    let param_ins = set_up_params params in
    let body = param_ins @ List.concat_map gen_instruction body in
    Function { name; global; body }
  | Tacky.Ast.StaticVar { name; global; tp; init_list } ->
    let alignment =
      match get_asm_type_for_c_type tp with
      | Byte -> 1
      | Word -> 2
      | DWord -> 4
      | QWord -> 8
      | AsmDouble -> 8
      | ByteArray { alignment; _ } -> alignment
    in
    StaticVar { name; global; alignment; init_list }
;;

let gen_program = function
  | Tacky.Ast.Program tpns ->
    let tpns = List.map gen_top_level tpns in
    let acc =
      Hashtbl.fold
        (fun (ci, alignment) lbl acc ->
           let tp_ast =
             StaticConstant
               { name = Identifier lbl
               ; alignment
               ; init = DoubleInit (Stdlib.Int64.float_of_bits ci)
               }
           in
           tp_ast :: acc)
        static_consts
        tpns
    in
    Program acc
;;
