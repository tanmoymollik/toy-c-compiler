open Ast

let build_graph _ = ()
(* let interference_graph = base_graph in
  add_pseudo_regs interference_graph instructions;
  let cfg = X64Cfg.make_control_flow_graph instructions in
  analyze_liveness cfg;
  add_edges cfg interference_graph;
  interference_graph *)

let add_spill_costs _ _ = ()
let color_graph _ = ()
let create_register_map g = g

let allocate_registers body =
  let interference_graph = build_graph body in
  add_spill_costs interference_graph body;
  color_graph interference_graph;
  let register_map = create_register_map interference_graph in
  register_map
;;

let resolve_top_level = function
  | Function { name; global; body } ->
    let register_map = allocate_registers body in
    let converted_body = PseudoResolver.resolve_instructions name body register_map in
    Function { name; global; body = converted_body }
  | (StaticConstant _ | StaticVar _) as ret -> ret
;;
