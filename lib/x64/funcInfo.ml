open Ast
open Common

let func_params : (string, reg list * reg list) Hashtbl.t = Hashtbl.create 10
let func_callee_saved_regs : (string, reg list) Hashtbl.t = Hashtbl.create 10

let save_func_params func_name int_reg_params double_reg_params =
  match func_name with
  | Identifier name -> Hashtbl.replace func_params name (int_reg_params, double_reg_params)
;;

let get_func_params = function
  | Identifier name ->
    (match Hashtbl.find_opt func_params name with
     | Some v -> v
     | None -> failwith "Function params should have been recorded")
;;

let save_callee_saved_regs func_name regs =
  match func_name with
  | Identifier name -> Hashtbl.replace func_callee_saved_regs name regs
;;

let get_callee_saved_regs = function
  | Identifier name ->
    (match Hashtbl.find_opt func_callee_saved_regs name with
     | Some v -> v
     | None -> [])
;;
(* TODO: Make this default after finishing part 2. *)
(* | None -> failwith "Function callee-saved registers should have been recorded") *)
