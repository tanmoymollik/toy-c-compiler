open Optimizations

let make_control_flow_graph func_body = func_body
let fold_constants func_body = func_body, false
let propagate_copies func_body = func_body, false
let eliminate_unreachable_code func_body = func_body, false
let eliminate_dead_stores func_body = func_body, false

(* Recursively optimizes the func_body.
   func_body must be a Tacky.Ast.Function. *)
let rec optimize func_body optimizations =
  let should_fold_constants =
    List.exists
      (function
        | FoldConstants -> true
        | _ -> false)
      optimizations
  in
  let post_constant_folding, cont =
    if should_fold_constants then fold_constants func_body else func_body, false
  in
  let cfg = make_control_flow_graph post_constant_folding in
  let cfg, cont =
    List.fold_left
      (fun (cfg, cont) opt ->
         let cfg, ncont =
           match opt with
           | FoldConstants -> cfg, cont
           | PropagateCopies -> propagate_copies cfg
           | EliminateUnreachableCode -> eliminate_unreachable_code cfg
           | EliminateDeadStores -> eliminate_dead_stores cfg
         in
         cfg, cont || ncont)
      (cfg, cont)
      optimizations
  in
  if cont then optimize cfg optimizations else cfg
;;
