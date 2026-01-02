open Optimizations
open TackyCfg

let eliminate_dead_stores _ = false

(* Recursively optimizes the func_body.
   func_body must be a Tacky.Ast.Function. *)
let rec optimize_impl func_body optimizations =
  let should_fold_constants =
    List.exists
      (function
        | FoldConstants -> true
        | _ -> false)
      optimizations
  in
  let post_constant_folding, cont =
    if should_fold_constants
    then ConstFolder.fold_constants func_body
    else func_body, false
  in
  let cfg = TackyCfg.make_control_flow_graph post_constant_folding in
  let cont =
    List.fold_left
      (fun cont opt ->
         let ncont =
           match opt with
           | FoldConstants -> cont
           | PropagateCopies -> CopyPropagater.propagate_copies cfg
           | EliminateUnreachableCode -> CodeRemover.eliminate_unreachable_code cfg
           | EliminateDeadStores -> eliminate_dead_stores cfg
         in
         cont || ncont)
      cont
      optimizations
  in
  let func_body = TackyCfg.make_body cfg in
  if cont then optimize_impl func_body optimizations else func_body
;;

let optimize optimizations = function
  | Tacky.Ast.Program tpns ->
    let tpns =
      List.map
        (function
          | Tacky.Ast.Function { name; global; params; body } ->
            Tacky.Ast.Function
              { name; global; params; body = optimize_impl body optimizations }
          | x -> x)
        tpns
    in
    Tacky.Ast.Program tpns
;;
