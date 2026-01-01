open Common

type generic_ins =
  | Return
  | Jump of identifier
  | CondJump of identifier
  | Label of identifier
  | Other
[@@deriving show]

module type Instruction = sig
  type elm

  val convert : elm -> generic_ins
  val pp_elm : Format.formatter -> elm -> unit
end

module MakeCfg (I : Instruction) = struct
  type node_id =
    | Entry
    | Exit
    | BlockId of int
  [@@deriving show]

  type node =
    | BasicBlock of
        { id : node_id
        ; ins : I.elm list
        ; pred : node_id list
        ; succ : node_id list
        }
    | EntryNode of { succ : node_id list }
    | ExitNode of { pred : node_id list }
  [@@deriving show]

  type graph =
    { nodes : (node_id, node) Hashtbl.t
    ; basic_blocks : int
    }

  let show_graph = function
    | { nodes; basic_blocks } ->
      let entry = show_node (Hashtbl.find nodes Entry) in
      let exit = show_node (Hashtbl.find nodes Exit) in
      let nids = List.init basic_blocks (fun i -> BlockId i) in
      let nd_strs =
        List.filter_map
          (fun nid ->
             match Hashtbl.find_opt nodes nid with
             | Some nd -> Some (show_node nd)
             | None -> None)
          nids
      in
      (entry ^ "\n") ^ String.concat "\n" nd_strs ^ "\n" ^ exit
  ;;

  let partition_into_basic_blocks ins =
    let blocks = ref [] in
    let curr_block = ref [] in
    let loop_fun itm =
      match I.convert itm with
      | Return | Jump _ | CondJump _ ->
        curr_block := itm :: !curr_block;
        blocks := List.rev !curr_block :: !blocks;
        curr_block := []
      | Label _ ->
        if not (List.is_empty !curr_block) then blocks := List.rev !curr_block :: !blocks;
        curr_block := [ itm ]
      | Other -> curr_block := itm :: !curr_block
    in
    List.iter loop_fun ins;
    if not (List.is_empty !curr_block) then blocks := List.rev !curr_block :: !blocks;
    List.rev !blocks
  ;;

  let id_of = function
    | EntryNode _ -> Entry
    | ExitNode _ -> Exit
    | BasicBlock { id; _ } -> id
  ;;

  let get_node g nid = Hashtbl.find g.nodes nid

  let add_succ nd1 nid2 =
    match nd1 with
    | EntryNode { succ } -> EntryNode { succ = nid2 :: succ }
    | BasicBlock { id; ins; pred; succ } ->
      BasicBlock { id; ins; pred; succ = nid2 :: succ }
    | ExitNode _ -> assert false
  ;;

  let remove_succ nd1 nid2 =
    let f succ nid = List.filter (fun n -> n <> nid) succ in
    match nd1 with
    | EntryNode { succ } -> EntryNode { succ = f succ nid2 }
    | BasicBlock { id; ins; pred; succ } ->
      BasicBlock { id; ins; pred; succ = f succ nid2 }
    | ExitNode _ -> assert false
  ;;

  let add_pred nd1 nid2 =
    match nd1 with
    | EntryNode _ -> assert false
    | BasicBlock { id; ins; pred; succ } ->
      BasicBlock { id; ins; pred = nid2 :: pred; succ }
    | ExitNode { pred } -> ExitNode { pred = nid2 :: pred }
  ;;

  let remove_pred nd1 nid2 =
    let f pred nid = List.filter (fun n -> n <> nid) pred in
    match nd1 with
    | EntryNode _ -> assert false
    | BasicBlock { id; ins; pred; succ } ->
      BasicBlock { id; ins; pred = f pred nid2; succ }
    | ExitNode { pred } -> ExitNode { pred = f pred nid2 }
  ;;

  let add_edge g nid1 nid2 =
    Hashtbl.replace g.nodes nid1 (add_succ (get_node g nid1) nid2);
    Hashtbl.replace g.nodes nid2 (add_pred (get_node g nid2) nid1)
  ;;

  let add_edges g nids1 nids2 =
    List.iter (fun nid1 -> List.iter (fun nid2 -> add_edge g nid1 nid2) nids2) nids1
  ;;

  let get_first g nid =
    let nd = get_node g nid in
    match nd with
    | BasicBlock { ins; _ } -> List.hd ins
    | EntryNode _ | ExitNode _ -> assert false
  ;;

  let get_last g nid =
    let nd = get_node g nid in
    match nd with
    | BasicBlock { ins; _ } -> List.hd (List.rev ins)
    | EntryNode _ | ExitNode _ -> assert false
  ;;

  let make_graph blocks =
    let nodes = Hashtbl.create 100 in
    Hashtbl.add nodes Entry (EntryNode { succ = [] });
    List.iteri
      (fun i ins ->
         let nid = BlockId i in
         Hashtbl.add nodes nid (BasicBlock { id = nid; ins; pred = []; succ = [] }))
      blocks;
    Hashtbl.add nodes Exit (ExitNode { pred = [] });
    { nodes; basic_blocks = List.length blocks }
  ;;

  (* TODO: Use a hashtable for better performance. *)
  let get_block_by_label g lbl =
    let node_cnt = g.basic_blocks in
    let loop_itr ind =
      let instr = get_first g (BlockId ind) in
      let instr = I.convert instr in
      match instr with
      | Label tgt -> lbl = tgt
      | _ -> false
    in
    let i = List.find loop_itr (List.init node_cnt (fun i -> i)) in
    BlockId i
  ;;

  let add_all_edges g =
    let node_cnt = g.basic_blocks in
    (* Function must have at least a return instruction. So there must
       be at least 1 block. *)
    add_edge g Entry (BlockId 0);
    let loop_itr ind =
      let next_id = if ind + 1 = node_cnt then Exit else BlockId (ind + 1) in
      let nid = BlockId ind in
      let instr = get_last g nid in
      let instr = I.convert instr in
      match instr with
      | Return -> add_edge g nid Exit
      | Jump tgt ->
        let tgt_id = get_block_by_label g tgt in
        add_edge g nid tgt_id
      | CondJump tgt ->
        let tgt_id = get_block_by_label g tgt in
        add_edge g nid tgt_id;
        add_edge g nid next_id
      | _ -> add_edge g nid next_id
    in
    let _ = List.init node_cnt loop_itr in
    ()
  ;;

  let make_control_flow_graph ins =
    let blocks = partition_into_basic_blocks ins in
    let g = make_graph blocks in
    add_all_edges g;
    g
  ;;

  let remove_basic_block g bi =
    let nodes = g.nodes in
    let nid = BlockId bi in
    match Hashtbl.find_opt nodes (BlockId bi) with
    | Some (BasicBlock { succ; pred; _ }) ->
      List.iter
        (fun sid ->
           let sd = remove_pred (get_node g sid) nid in
           Hashtbl.replace nodes sid sd)
        succ;
      List.iter
        (fun pid ->
           let pd = remove_succ (get_node g pid) nid in
           Hashtbl.replace nodes pid pd)
        pred;
      Hashtbl.remove nodes nid
    | Some (EntryNode _ | ExitNode _) -> assert false
    | None -> ()
  ;;

  let make_body = function
    | { nodes; basic_blocks } ->
      let loop_itr id =
        match Hashtbl.find_opt nodes id with
        | Some (BasicBlock { ins; _ }) -> ins
        | Some (EntryNode _ | ExitNode _) -> assert false
        | None -> []
      in
      List.concat_map loop_itr (List.init basic_blocks (fun i -> BlockId i))
  ;;
end
