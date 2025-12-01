(* Converts to c int which is 32 bits wide. *)
let convert_to_int = function
  | C_ast.ConstInt i -> i
  | C_ast.ConstLong l -> Int64.to_int32 l
;;

(* Converts to c long which is 64 bits wide. *)
let convert_to_long = function
  | C_ast.ConstInt i -> Int64.of_int32 i
  | C_ast.ConstLong l -> l
;;
