open Common

let get_oprnd_size_for_c_type = function
  | C_ast.Int | C_ast.UInt -> DWord
  | C_ast.Long | C_ast.ULong -> QWord
  | C_ast.FunType _ -> assert false
;;

let get_oprnd_size_for_iden iden =
  let ctp = Symbol_map.get_var_type iden in
  get_oprnd_size_for_c_type ctp
;;

let get_oprnd_size_for_val = function
  | Tacky.Constant (ConstInt _ | ConstUInt _) -> DWord
  | Tacky.Constant (ConstLong _ | ConstULong _) -> QWord
  | Tacky.Var iden -> get_oprnd_size_for_iden iden
;;

let get_oprnd_size_for_fun_ret iden =
  let ctp = Symbol_map.get_fun_ret_type iden in
  get_oprnd_size_for_c_type ctp
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
         match get_oprnd_size_for_iden var_iden with
         | DWord -> 4
         | QWord -> 8
         | _ -> assert false
       in
       let stk_ptr = get_fun_stack_alloc fun_iden + b_sz in
       Hashtbl.add fun_map fun_name stk_ptr;
       Hashtbl.add var_map var_name stk_ptr;
       stk_ptr)
;;
