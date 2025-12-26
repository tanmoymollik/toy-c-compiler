open Tacky.Ast
open TypeConverter

let process_instruction = function
  | Unary { uop; src = Constant c; dst } ->
    let c = evaluate_unary uop c in
    Some (Copy { src = Constant c; dst }), true
  | Binary { bop; src1 = Constant c1; src2 = Constant c2; dst } ->
    let c = evaluate_binary bop c1 c2 in
    Some (Copy { src = Constant c; dst }), true
  | JumpIfZero (Constant c, tgt) -> (if is_zero c then Some (Jump tgt) else None), true
  | JumpIfNotZero (Constant c, tgt) -> (if is_zero c then None else Some (Jump tgt)), true
  | ins -> Some ins, false
;;

let fold_constants func_body =
  let changed = ref false in
  let func_body =
    List.filter_map
      (fun ins ->
         let ins, fl = process_instruction ins in
         changed := !changed || fl;
         ins)
      func_body
  in
  func_body, !changed
;;
