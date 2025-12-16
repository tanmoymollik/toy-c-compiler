open Common

let get_oprnd_size_for_val = function
  | Tacky.Constant (ConstInt _ | ConstUInt _) -> DWord
  | Tacky.Constant (ConstLong _ | ConstULong _ | ConstDouble _) -> QWord
  | Tacky.Var iden -> AsmSymbolMap.get_var_type iden
;;

let var_map : (string, int) Hashtbl.t = Hashtbl.create 100
let fun_map : (string, int) Hashtbl.t = Hashtbl.create 100

let get_fun_stack_alloc = function
  | Identifier fun_name ->
    (match Hashtbl.find_opt fun_map fun_name with
     | Some v -> v
     | None -> 0)
;;

let get_stack_address = function
  | (Identifier fun_name as fun_iden), (Identifier var_name as var_iden) ->
    (match Hashtbl.find_opt var_map var_name with
     | Some v -> v
     | None ->
       let b_sz =
         match AsmSymbolMap.get_var_type var_iden with
         | DWord -> 4
         | QWord -> 8
         | _ -> assert false
       in
       let stk_ptr = get_fun_stack_alloc fun_iden + b_sz in
       Hashtbl.add fun_map fun_name stk_ptr;
       Hashtbl.add var_map var_name stk_ptr;
       stk_ptr)
;;
