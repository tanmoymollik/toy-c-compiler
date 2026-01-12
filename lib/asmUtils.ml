open Common

let var_map : (string, int) Hashtbl.t = Hashtbl.create 100
let fun_map : (string, int) Hashtbl.t = Hashtbl.create 100

let get_fun_stack_alloc = function
  | Identifier fun_name ->
    (match Hashtbl.find_opt fun_map fun_name with
     | Some v -> v
     | None -> 0)
;;

let align_by addr base =
  assert (base > 0);
  let r = addr mod base in
  if r > 0 then addr + (base - r) else addr
;;

let get_stack_address = function
  | (Identifier fun_name as fun_iden), (Identifier var_name as var_iden) ->
    (match Hashtbl.find_opt var_map var_name with
     | Some v -> v
     | None ->
       let asm_tp = AsmSymbolMap.get_var_type var_iden in
       let b_sz = size_for_asm_type asm_tp in
       let alignment = alignment_for_asm_type asm_tp in
       let stk_ptr = get_fun_stack_alloc fun_iden + b_sz in
       let stk_ptr = align_by stk_ptr alignment in
       Hashtbl.replace fun_map fun_name stk_ptr;
       Hashtbl.replace var_map var_name stk_ptr;
       stk_ptr)
;;

let get_asm_type_for_val = function
  | Tacky.Ast.Constant c -> get_asm_type_for_const c
  | Tacky.Ast.Var iden -> AsmSymbolMap.get_var_type iden
;;
