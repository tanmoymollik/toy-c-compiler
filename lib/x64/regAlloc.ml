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

let are_neighbors ig nid1 nid2 =
  let nd1 = Hashtbl.find ig.nodes nid1 in
  match Hashtbl.find_opt nd1.neighbors nid2 with
  | Some _ -> true
  | None -> false
;;

let is_hard_reg = function
  | Reg _ -> true
  | _ -> false
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

let remove_neighbor ig nid1 nid2 =
  let nd1 = Hashtbl.find ig.nodes nid1 in
  Hashtbl.remove nd1.neighbors nid2
;;

let add_edge ig nid1 nid2 =
  assert (is_in_graph ig nid1);
  assert (is_in_graph ig nid2);
  add_neighbor ig nid1 nid2;
  add_neighbor ig nid2 nid1
;;

let remove_edge ig nid1 nid2 =
  assert (is_in_graph ig nid1);
  assert (is_in_graph ig nid2);
  remove_neighbor ig nid1 nid2;
  remove_neighbor ig nid2 nid1
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

let briggs_test ig k x y =
  let x_node = Hashtbl.find ig.nodes x in
  let y_node = Hashtbl.find ig.nodes y in
  let combined_neighbors = SetOp.union x_node.neighbors y_node.neighbors in
  let significant_neighbors =
    Hashtbl.fold
      (fun nid _ acc ->
         let neighbor_node = Hashtbl.find ig.nodes nid in
         let degree = node_degree ig neighbor_node in
         let degree =
           if are_neighbors ig nid x && are_neighbors ig nid y then degree - 1 else degree
         in
         if degree >= k then acc + 1 else acc)
      combined_neighbors
      0
  in
  significant_neighbors < k
;;

let george_test ig k hardreg pseudoreg =
  let pseudo_node = Hashtbl.find ig.nodes pseudoreg in
  Hashtbl.fold
    (fun nid _ acc ->
       if are_neighbors ig nid hardreg
       then acc
       else (
         let neighbor_node = Hashtbl.find ig.nodes nid in
         if Hashtbl.length neighbor_node.neighbors < k then acc else false))
    pseudo_node.neighbors
    true
;;

let conservative_coalesceable ig k src dst =
  if briggs_test ig k src dst
  then true
  else if is_hard_reg src
  then george_test ig k src dst
  else if is_hard_reg dst
  then george_test ig k dst src
  else false
;;

let update_graph ig to_merge to_keep =
  let node_to_remove = Hashtbl.find ig.nodes to_merge in
  Hashtbl.iter
    (fun nid _ ->
       add_edge ig to_keep nid;
       remove_neighbor ig nid to_merge)
    node_to_remove.neighbors;
  Hashtbl.remove ig.nodes to_merge
;;

let coalesce ig k instructions =
  let coalesced_regs = DisjointSet.init_disjoint_sets () in
  List.iter
    (function
      | Mov { src; dst; _ } ->
        let src = DisjointSet.find coalesced_regs src in
        let dst = DisjointSet.find coalesced_regs dst in
        if
          is_in_graph ig src
          && is_in_graph ig dst
          && src <> dst
          && are_neighbors ig src dst |> not
          && conservative_coalesceable ig k src dst
        then (
          let to_keep, to_merge = if is_hard_reg src then src, dst else dst, src in
          DisjointSet.union coalesced_regs to_merge to_keep;
          update_graph ig to_merge to_keep)
      | _ -> ())
    instructions;
  coalesced_regs
;;

let rewrite_instruction reg_map = function
  | Mov { src; dst; tp } ->
    let src = DisjointSet.find reg_map src in
    let dst = DisjointSet.find reg_map dst in
    if src = dst then None else Some (Mov { src; dst; tp })
  | Unary (uop, dst, tp) -> Some (Unary (uop, DisjointSet.find reg_map dst, tp))
  | Binary { bop; src; dst; tp } ->
    let src = DisjointSet.find reg_map src in
    let dst = DisjointSet.find reg_map dst in
    Some (Binary { bop; src; dst; tp })
  | Cmp { lhs; rhs; tp } ->
    let lhs = DisjointSet.find reg_map lhs in
    let rhs = DisjointSet.find reg_map rhs in
    Some (Cmp { lhs; rhs; tp })
  | Idiv (src, tp) -> Some (Idiv (DisjointSet.find reg_map src, tp))
  | SetC (cc, dst) -> Some (SetC (cc, DisjointSet.find reg_map dst))
  | Push src -> Some (Push (DisjointSet.find reg_map src))
  | x -> Some x
;;

let rewrite_coalesced reg_map instructions =
  List.filter_map (rewrite_instruction reg_map) instructions
;;

let allocate_registers func_name body regalloc =
  if regalloc
  then (
    let k = 12 in
    let rec loop instructions =
      let interference_graph = build_graph instructions in
      let coalesced_regs = coalesce interference_graph k instructions in
      if DisjointSet.nothing_was_coalesced coalesced_regs
      then interference_graph, instructions
      else (
        let instructions = rewrite_coalesced coalesced_regs instructions in
        loop instructions)
    in
    let interference_graph, body = loop body in
    add_spill_costs interference_graph body;
    color_graph interference_graph k;
    let register_map = create_register_map func_name interference_graph k in
    PseudoResolver.resolve_instructions func_name body register_map)
  else PseudoResolver.resolve_instructions func_name body (Hashtbl.create 0)
;;

let resolve_top_level regalloc = function
  | Function { name; global; body } ->
    let converted_body = allocate_registers name body regalloc in
    Function { name; global; body = converted_body }
  | (StaticConstant _ | StaticVar _) as ret -> ret
;;
