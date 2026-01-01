open TackyCfg

let is_visited visited = function
  | TackyCfg.Entry | TackyCfg.Exit -> true
  | TackyCfg.BlockId i -> visited.(i)
;;

let rec dfs_loop visited nodes nid =
  if not (is_visited visited nid)
  then (
    let nd =
      match Hashtbl.find_opt nodes nid with
      | Some v -> v
      | None -> assert false
    in
    dfs visited nodes nd)

and dfs visited nodes = function
  | TackyCfg.EntryNode { succ } -> List.iter (dfs_loop visited nodes) succ
  | TackyCfg.BasicBlock { id; succ; _ } ->
    let id =
      match id with
      | BlockId id -> id
      | _ -> assert false
    in
    visited.(id) <- true;
    List.iter (dfs_loop visited nodes) succ
  | TackyCfg.ExitNode _ -> ()
;;

let prune_branches (g : TackyCfg.graph) =
  let visited = Array.make g.basic_blocks false in
  dfs visited g.nodes (Hashtbl.find g.nodes TackyCfg.Entry);
  let rfl = ref false in
  Array.iteri
    (fun ind fl ->
       rfl := !rfl || not fl;
       if not fl then TackyCfg.remove_basic_block g ind)
    visited;
  !rfl
;;

let sort_graph = function
  | TackyCfg.{ nodes; basic_blocks } ->
    let nids = List.init basic_blocks (fun i -> TackyCfg.BlockId i) in
    let nids =
      List.filter_map
        (fun nid ->
           match Hashtbl.find_opt nodes nid with
           | Some _ -> Some nid
           | None -> None)
        nids
    in
    nids
;;

let remove_redundant_jump_inner (g : TackyCfg.graph) sorted_graph =
  let sorted_graph = List.rev sorted_graph in
  let fl = ref false in
  let default_succ = ref (List.hd sorted_graph) in
  List.iter
    (fun nid ->
       match Hashtbl.find_opt g.nodes nid with
       | Some (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) | None -> assert false
       | Some (TackyCfg.BasicBlock { id; ins; succ; pred }) ->
         let last_ins = TackyInstruction.strip_annotation (List.hd (List.rev ins)) in
         (match last_ins with
          | Tacky.Ast.(Jump _ | JumpIfNotZero _ | JumpIfZero _) ->
            let keep_jump = List.exists (fun s -> s <> !default_succ) succ in
            fl := !fl || not keep_jump;
            let ins = if keep_jump then ins else List.rev (List.tl (List.rev ins)) in
            if List.length ins > 0
            then (
              default_succ := nid;
              let nd = TackyCfg.BasicBlock { id; ins; succ; pred } in
              Hashtbl.replace g.nodes nid nd)
            else (
              let id =
                match nid with
                | BlockId i -> i
                | _ -> assert false
              in
              TackyCfg.remove_basic_block g id;
              TackyCfg.add_edges g pred succ)
          | _ -> default_succ := nid))
    (List.tl sorted_graph);
  !fl
;;

let remove_redundant_jump g =
  let sorted_graph = sort_graph g in
  if List.length sorted_graph < 2
  then false
  else remove_redundant_jump_inner g sorted_graph
;;

let remove_redundant_label_inner (g : TackyCfg.graph) sorted_graph =
  let fl = ref false in
  let default_pred = ref TackyCfg.Entry in
  List.iter
    (fun nid ->
       match Hashtbl.find_opt g.nodes nid with
       | Some (TackyCfg.BasicBlock { id; ins; succ; pred }) ->
         let first_ins = TackyInstruction.strip_annotation (List.hd ins) in
         (match first_ins with
          | Tacky.Ast.Label _ ->
            let keep_label = List.exists (fun s -> s <> !default_pred) pred in
            fl := !fl || not keep_label;
            let ins = if keep_label then ins else List.tl ins in
            if List.length ins > 0
            then (
              default_pred := nid;
              let nd = TackyCfg.BasicBlock { id; ins; succ; pred } in
              Hashtbl.replace g.nodes nid nd)
            else (
              let id =
                match nid with
                | BlockId i -> i
                | _ -> assert false
              in
              TackyCfg.remove_basic_block g id;
              TackyCfg.add_edges g pred succ)
          | _ -> default_pred := nid)
       | Some (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) | None -> assert false)
    sorted_graph;
  !fl
;;

let remove_redundant_label g =
  let sorted_graph = sort_graph g in
  remove_redundant_label_inner g sorted_graph
;;

let rec remove_redundant_jump_and_label g =
  let fl1 = remove_redundant_jump g in
  let fl2 = remove_redundant_label g in
  if fl1 || fl2
  then (
    let _ = remove_redundant_jump_and_label g in
    true)
  else false
;;

let eliminate_unreachable_code (cfg : TackyCfg.graph) =
  let fl1 = prune_branches cfg in
  let fl2 = remove_redundant_jump_and_label cfg in
  fl1 || fl2
;;
