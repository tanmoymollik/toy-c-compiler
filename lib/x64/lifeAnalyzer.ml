open X64Cfg
open Ast
open SetOp

type annotation = (operand, unit) Hashtbl.t

let pp_annotation fmt v =
  let acc =
    Hashtbl.fold
      (fun k _ acc -> show_operand k ^ if acc = "" then acc else ", " ^ acc)
      v
      ""
  in
  Format.fprintf fmt "annotation:\n[\n%s\n]" acc
;;

let show_annotation v = Format.asprintf "%a" pp_annotation v
let block_ann_table = ref (Hashtbl.create 0)
let block_ins_table = ref (Hashtbl.create 0)
let annotate_block nid copies = Hashtbl.replace !block_ann_table nid copies

let get_block_annotation nid =
  let copies = Hashtbl.find !block_ann_table nid in
  Hashtbl.copy copies
;;

let annotate_instructions nid copies_list =
  Hashtbl.replace !block_ins_table nid copies_list
;;

let get_instructions_annotation nid =
  let copies_list = Hashtbl.find !block_ins_table nid in
  copies_list
;;

let meet block =
  match block with
  | X64Cfg.EntryNode _ | X64Cfg.ExitNode _ -> assert false
  | X64Cfg.BasicBlock { succ; _ } ->
    List.fold_left
      (fun acc sid ->
         match sid with
         | X64Cfg.Entry -> assert false
         | X64Cfg.Exit ->
           Hashtbl.replace acc (Reg Ax) ();
           acc
         | X64Cfg.BlockId _ ->
           let succ_live_vars = get_block_annotation sid in
           union acc succ_live_vars)
      (Hashtbl.create 0)
      succ
;;

let find_used_and_updated = function
  | Mov { src; dst; _ } -> [ src ], [ dst ]
  | Unary (_, dst, _) -> [ dst ], [ dst ]
  | Binary { src; dst; _ } -> [ src; dst ], [ dst ]
  | Cmp { lhs; rhs; _ } -> [ lhs; rhs ], []
  | SetC (_, dst) -> [], [ dst ]
  | Push v -> [ v ], []
  | Idiv (v, _) -> [ v; Reg Ax; Reg Dx ], [ Reg Ax; Reg Dx ]
  | Cdq _ -> [ Reg Ax ], [ Reg Dx ]
  | Call name ->
    let int_params, _ = FuncInfo.get_func_params name in
    let reg_to_oprnd lst = List.map (fun x -> Reg x) lst in
    reg_to_oprnd int_params, Reg Ax :: reg_to_oprnd int_regs
  | _ -> [], []
;;

let transfer block live_vars =
  let is_reg = function
    | Reg _ | Pseudo _ -> true
    | _ -> false
  in
  match block with
  | X64Cfg.EntryNode _ | X64Cfg.ExitNode _ -> assert false
  | X64Cfg.BasicBlock { id; ins; _ } ->
    let ins_annot =
      List.rev_map
        (fun ins ->
           let ret = Hashtbl.copy live_vars in
           let used, updated = find_used_and_updated ins in
           List.iter (fun x -> if is_reg x then Hashtbl.remove live_vars x) updated;
           List.iter (fun x -> if is_reg x then Hashtbl.replace live_vars x ()) used;
           ret)
        (List.rev ins)
    in
    annotate_block id (Hashtbl.copy live_vars);
    annotate_instructions id ins_annot
;;

let find_reaching_vars (g : X64Cfg.graph) =
  let workqueue = Queue.create () in
  let _ =
    List.init g.basic_blocks (fun i ->
      (* loop in reverse order. *)
      let i = g.basic_blocks - i - 1 in
      let nid = X64Cfg.BlockId i in
      match Hashtbl.find_opt g.nodes nid with
      | Some (X64Cfg.EntryNode _ | X64Cfg.ExitNode _) -> assert false
      | Some (X64Cfg.BasicBlock _) ->
        Queue.add nid workqueue;
        annotate_block nid (Hashtbl.create 0)
      | None -> ())
  in
  ();
  (* Loop queue until empty. *)
  let rec loop () =
    if not (Queue.is_empty workqueue)
    then (
      let nid = Queue.take workqueue in
      run nid;
      loop ())
  and run nid =
    let old_annotation = get_block_annotation nid in
    match Hashtbl.find g.nodes nid with
    | X64Cfg.EntryNode _ | X64Cfg.ExitNode _ -> assert false
    | X64Cfg.BasicBlock { pred; _ } as block ->
      let end_live_vars = meet block in
      transfer block end_live_vars;
      if not (equal old_annotation (get_block_annotation nid))
      then
        List.iter
          (function
            | X64Cfg.Entry -> ()
            | X64Cfg.Exit -> assert false
            | X64Cfg.BlockId _ as pid ->
              let exists_in_queue =
                Queue.fold (fun acc x -> if x = pid then true else acc) false workqueue
              in
              if not exists_in_queue then Queue.add pid workqueue)
          pred
  in
  loop ()
;;

let analyze_liveness (cfg : X64Cfg.graph) =
  block_ann_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  block_ins_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  find_reaching_vars cfg
;;
