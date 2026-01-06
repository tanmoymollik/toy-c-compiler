open Ast
open Common
open X64Cfg

let inf = Int.max_int

type node =
  { id : operand
  ; neighbors : (operand, unit) Hashtbl.t
  ; spill_cost : int
  ; color : int option
  ; pruned : bool
  }

type graph = { nodes : (operand, node) Hashtbl.t }

let show_node nd =
  let indent = "    " in
  print_endline ("id: " ^ show_operand nd.id);
  print_endline ("spill_cost: " ^ Int.to_string nd.spill_cost);
  print_endline
    ("neighbors:\n"
     ^ Hashtbl.fold
         (fun k _ acc -> indent ^ show_operand k ^ if acc = "" then acc else ",\n" ^ acc)
         nd.neighbors
         "");
  print_endline
    ("color: " ^ if nd.color = None then "None" else Option.get nd.color |> Int.to_string);
  print_endline ("pruned: " ^ Bool.to_string nd.pruned)
;;

let show_graph ig =
  Hashtbl.iter
    (fun k v ->
       print_endline ("operand: " ^ show_operand k);
       print_endline "node------------------: ";
       show_node v;
       print_endline "")
    ig.nodes
;;

let show_register_map rm =
  Hashtbl.iter
    (fun k v ->
       print_endline ("identifier: " ^ show_identifier k);
       print_endline ("reg: " ^ show_reg v))
    rm
;;

let is_in_graph ig oprnd =
  match Hashtbl.find_opt ig.nodes oprnd with
  | Some _ -> true
  | None -> false
;;

let base_graph () =
  let regs = [ Ax; Bx; Cx; Dx; Di; Si; R8; R9; R12; R13; R14; R15 ] in
  let nodes = Hashtbl.create (List.length regs) in
  List.iter
    (fun r ->
       let id = Reg r in
       let neighbors =
         let v = Hashtbl.create (List.length regs) in
         List.iter (fun x -> if x = r then () else Hashtbl.replace v (Reg x) ()) regs;
         v
       in
       let node = { id; neighbors; spill_cost = inf; color = None; pruned = false } in
       Hashtbl.add nodes id node)
    regs;
  { nodes }
;;

let add_pseudo_regs ig instructions =
  let add oprnd =
    match oprnd with
    | Pseudo p ->
      if
        (not (is_in_graph ig oprnd))
        && AsmSymbolMap.get_var_type p <> AsmDouble
        && not (AsmSymbolMap.is_static_var p)
      then
        Hashtbl.add
          ig.nodes
          oprnd
          { id = oprnd
          ; neighbors = Hashtbl.create 5
          ; spill_cost = 0
          ; color = None
          ; pruned = false
          }
    | _ -> ()
  in
  List.iter
    (function
      | Mov { src; dst; _ } | Binary { src; dst; _ } ->
        add src;
        add dst
      | Cmp { lhs; rhs; _ } ->
        add lhs;
        add rhs
      | Unary (_, dst, _) | SetC (_, dst) -> add dst
      | Idiv (src, _) | Div (src, _) | Push src -> add src
      | _ -> ())
    instructions
;;

let add_neighbor ig nid1 nid2 =
  let nd1 = Hashtbl.find ig.nodes nid1 in
  Hashtbl.replace nd1.neighbors nid2 ()
;;

let add_edge ig nid1 nid2 =
  assert (is_in_graph ig nid1);
  assert (is_in_graph ig nid2);
  add_neighbor ig nid1 nid2;
  add_neighbor ig nid2 nid1
;;

let add_edges (cfg : X64Cfg.graph) ig =
  Hashtbl.iter
    (fun id -> function
       | X64Cfg.EntryNode _ | X64Cfg.ExitNode _ -> ()
       | X64Cfg.BasicBlock { ins; _ } ->
         let annots = LifeAnalyzer.get_instructions_annotation id in
         List.iter2
           (fun ins live_regs ->
              let _, updated = LifeAnalyzer.find_used_and_updated ins in
              Hashtbl.iter
                (fun k _ ->
                   match ins with
                   | Mov { src; _ } when k = src -> ()
                   | _ ->
                     List.iter
                       (fun u ->
                          if is_in_graph ig k && is_in_graph ig u && k <> u
                          then add_edge ig k u)
                       updated)
                live_regs)
           ins
           annots)
    cfg.nodes
;;

let build_graph instructions =
  let interference_graph = base_graph () in
  add_pseudo_regs interference_graph instructions;
  let cfg = X64Cfg.make_control_flow_graph instructions in
  LifeAnalyzer.analyze_liveness cfg;
  add_edges cfg interference_graph;
  interference_graph
;;

let count_occurance oprnd ins =
  let f a = if a = oprnd then 1 else 0 in
  match ins with
  | Mov { src; dst; _ } | Binary { src; dst; _ } -> f src + f dst
  | Unary (_, dst, _) | SetC (_, dst) -> f dst
  | Cmp { lhs; rhs; _ } -> f lhs + f rhs
  | Push src | Idiv (src, _) -> f src
  | _ -> 0
;;

let add_spill_costs ig instructions =
  Hashtbl.filter_map_inplace
    (fun id v ->
       match id with
       | Pseudo _ ->
         let o =
           List.fold_left (fun acc ins -> acc + count_occurance id ins) 0 instructions
         in
         let nd = { v with spill_cost = o } in
         Some nd
       | _ -> Some v)
    ig.nodes
;;

let node_degree ig = function
  | { neighbors; _ } ->
    Hashtbl.fold
      (fun k _ acc ->
         match Hashtbl.find_opt ig.nodes k with
         | Some { pruned = false; _ } -> acc + 1
         | _ -> acc)
      neighbors
      0
;;

let rec color_graph_inner ig k chosen_node =
  let chosen_node = { chosen_node with pruned = true } in
  Hashtbl.replace ig.nodes chosen_node.id chosen_node;
  color_graph ig k;
  let colors = Hashtbl.create k in
  for i = 1 to k do
    Hashtbl.add colors i ()
  done;
  Hashtbl.iter
    (fun nid _ ->
       let nd = Hashtbl.find ig.nodes nid in
       if nd.color <> None then Hashtbl.remove colors (Option.get nd.color))
    chosen_node.neighbors;
  if Hashtbl.length colors > 0
  then (
    let min, max =
      Hashtbl.fold (fun k _ (mi, mx) -> Int.min mi k, Int.max mx k) colors (k, 0)
    in
    let color =
      match chosen_node.id with
      | Reg r when is_callee_saved_reg r -> max
      | _ -> min
    in
    let chosen_node = { chosen_node with color = Some color; pruned = false } in
    Hashtbl.replace ig.nodes chosen_node.id chosen_node)

and color_graph ig k =
  let remaining =
    Hashtbl.fold (fun _ v acc -> if v.pruned then acc else v :: acc) ig.nodes []
  in
  let chosen_node = ref None in
  let rec loop = function
    | [] -> ()
    | hd :: tl ->
      let degree = node_degree ig hd in
      if degree < k then chosen_node := Some hd else loop tl
  in
  loop remaining;
  if !chosen_node = None
  then (
    let best_spill_metric = ref (Float.of_int inf) in
    List.iter
      (fun nd ->
         let degree = node_degree ig nd in
         let spill_metric =
           if degree = 0
           then Float.of_int inf
           else Float.of_int nd.spill_cost /. Float.of_int degree
         in
         if spill_metric < !best_spill_metric
         then (
           best_spill_metric := spill_metric;
           chosen_node := Some nd))
      remaining);
  if !chosen_node = None then () else color_graph_inner ig k (Option.get !chosen_node)
;;

let create_register_map func_name ig k =
  let color_map = Hashtbl.create k in
  Hashtbl.iter
    (fun k { color; _ } ->
       match k with
       | Reg r ->
         (match color with
          | Some color -> Hashtbl.replace color_map color r
          | None -> ())
       | _ -> ())
    ig.nodes;
  let register_map = Hashtbl.create (Hashtbl.length ig.nodes) in
  let callee_saved_regs = Hashtbl.create 5 in
  Hashtbl.iter
    (fun k { color; _ } ->
       match k with
       | Pseudo p when color <> None ->
         let color = Option.get color in
         let hardreg = Hashtbl.find color_map color in
         Hashtbl.replace register_map p hardreg;
         if is_callee_saved_reg hardreg then Hashtbl.replace callee_saved_regs hardreg ()
       | _ -> ())
    ig.nodes;
  let callee_saved_regs = Hashtbl.fold (fun k _ acc -> k :: acc) callee_saved_regs [] in
  FuncInfo.save_callee_saved_regs func_name callee_saved_regs;
  register_map
;;

let allocate_registers func_name body =
  let k = 12 in
  let interference_graph = build_graph body in
  add_spill_costs interference_graph body;
  color_graph interference_graph k;
  let register_map = create_register_map func_name interference_graph k in
  register_map
;;

let resolve_top_level = function
  | Function { name; global; body } ->
    (* TODO: Remove disabling regalloc. *)
    let disable_regalloc = false in
    let register_map = allocate_registers name body in
    let converted_body =
      PseudoResolver.resolve_instructions
        name
        body
        (if disable_regalloc then Hashtbl.create 0 else register_map)
    in
    Function { name; global; body = converted_body }
  | (StaticConstant _ | StaticVar _) as ret -> ret
;;
