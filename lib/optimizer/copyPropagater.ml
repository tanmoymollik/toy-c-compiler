open TackyCfg

let intersect ann1 ann2 =
  let res = Hashtbl.create (Hashtbl.length ann1) in
  Hashtbl.iter
    (fun k v ->
       match Hashtbl.find_opt ann2 k with
       | Some _ -> Hashtbl.add res k v
       | None -> ())
    ann1;
  res
;;

let equal ann1 ann2 =
  if Hashtbl.length ann1 <> Hashtbl.length ann2
  then false
  else (
    let cnt =
      Hashtbl.fold
        (fun k _ acc ->
           match Hashtbl.find_opt ann2 k with
           | Some _ -> acc + 1
           | None -> acc)
        ann1
        0
    in
    cnt = Hashtbl.length ann1)
;;

let annotate_block ann_table nid copies =
  Hashtbl.replace ann_table nid (Hashtbl.copy copies)
;;

let get_block_annotation ann_table nid =
  let copies = Hashtbl.find ann_table nid in
  Hashtbl.copy copies
;;

let meet ann_table block incoming_copies =
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { pred; _ } ->
    List.fold_left
      (fun acc pid ->
         match pid with
         | TackyCfg.Entry -> Hashtbl.create 0
         | TackyCfg.Exit -> assert false
         | TackyCfg.BlockId _ ->
           let pred_out_copies = get_block_annotation ann_table pid in
           intersect acc pred_out_copies)
      incoming_copies
      pred
;;

let is_static_value = function
  | Tacky.Ast.Constant _ -> false
  | Tacky.Ast.Var iden -> AsmSymbolMap.is_static_var iden
;;

let update_copies reaching_copies ins =
  let default_keep dst = function
    | Tacky.Ast.Copy { src = tsrc; dst = tdst } ->
      if tsrc = dst || tdst = dst then false else true
    | _ -> assert false
  in
  let update_copies_inner dst copies keep =
    Hashtbl.filter_map_inplace (fun k _ -> if keep dst k then Some () else None) copies
  in
  match ins with
  | Tacky.Ast.Copy { src; dst } as v ->
    let exists_rev =
      match
        Hashtbl.find_opt reaching_copies (Tacky.Ast.Copy { src = dst; dst = src })
      with
      | Some _ -> true
      | None -> false
    in
    if exists_rev
    then ()
    else (
      update_copies_inner dst reaching_copies default_keep;
      Hashtbl.replace reaching_copies v ())
  | Tacky.Ast.FunCall { dst; _ } ->
    let keep dst = function
      | Tacky.Ast.Copy { src = tsrc; dst = tdst } ->
        if is_static_value tsrc || is_static_value tdst || tsrc = dst || tdst = dst
        then false
        else true
      | _ -> assert false
    in
    update_copies_inner dst reaching_copies keep
  | Tacky.Ast.Unary { dst; _ } -> update_copies_inner dst reaching_copies default_keep
  | Tacky.Ast.Binary { dst; _ } -> update_copies_inner dst reaching_copies default_keep
  | _ -> ()
;;

let transfer ann_table block reaching_copies =
  let curr_reaching_copies = Hashtbl.copy reaching_copies in
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { id; ins; pred; succ } ->
    let ins =
      List.map
        (fun (ins, _) ->
           let ret = ins, Hashtbl.copy curr_reaching_copies in
           update_copies curr_reaching_copies ins;
           ret)
        ins
    in
    annotate_block ann_table id curr_reaching_copies;
    TackyCfg.BasicBlock { id; ins; pred; succ }
;;

let find_all_copy_ins (g : TackyCfg.graph) =
  let all_copies = Hashtbl.create 50 in
  Hashtbl.iter
    (fun _ -> function
       | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> ()
       | TackyCfg.BasicBlock { ins; _ } ->
         List.iter
           (fun (ins, _) ->
              match ins with
              | Tacky.Ast.Copy _ as v -> Hashtbl.replace all_copies v ()
              | _ -> ())
           ins)
    g.nodes;
  all_copies
;;

let operand_changed = ref false

let replace_operand reaching_copies op =
  match op with
  | Tacky.Ast.Constant _ -> op
  | Tacky.Ast.Var _ ->
    Hashtbl.fold
      (fun k _ acc ->
         match k with
         | Tacky.Ast.Copy { src; dst } ->
           if dst = op
           then (
             operand_changed := true;
             src)
           else acc
         | _ -> assert false)
      reaching_copies
      op
;;

let rewrite_instruction = function
  | ins, reaching_copies ->
    (match ins with
     | Tacky.Ast.Ret r ->
       let ret = Tacky.Ast.Ret (replace_operand reaching_copies r) in
       Some ret
     | Tacky.Ast.Unary { uop; src; dst } ->
       let ret =
         Tacky.Ast.Unary { uop; src = replace_operand reaching_copies src; dst }
       in
       Some ret
     | Tacky.Ast.Binary { bop; src1; src2; dst } ->
       let ret =
         Tacky.Ast.Binary
           { bop
           ; src1 = replace_operand reaching_copies src1
           ; src2 = replace_operand reaching_copies src2
           ; dst
           }
       in
       Some ret
     | Tacky.Ast.Copy { src; dst } ->
       let new_instr =
         Some (Tacky.Ast.Copy { src = replace_operand reaching_copies src; dst })
       in
       Hashtbl.fold
         (fun k _ acc ->
            match k with
            | Tacky.Ast.Copy { src = tsrc; dst = tdst } ->
              if ins = k || (tsrc = dst && tdst = src) then None else acc
            | _ -> assert false)
         reaching_copies
         new_instr
     | Tacky.Ast.JumpIfZero (cnd, iden) ->
       let ret = Tacky.Ast.JumpIfZero (replace_operand reaching_copies cnd, iden) in
       Some ret
     | Tacky.Ast.JumpIfNotZero (cnd, iden) ->
       let ret = Tacky.Ast.JumpIfNotZero (replace_operand reaching_copies cnd, iden) in
       Some ret
     | Tacky.Ast.FunCall { name; args; dst } ->
       let ret =
         let args = List.map (replace_operand reaching_copies) args in
         Tacky.Ast.FunCall { name; args; dst }
       in
       Some ret
     | (Tacky.Ast.Jump _ | Tacky.Ast.Label _) as x -> Some x
     | _ -> assert false)
;;

let find_reaching_copies ann_table (g : TackyCfg.graph) =
  let all_copies = find_all_copy_ins g in
  let workqueue = Queue.create () in
  let _ =
    List.init g.basic_blocks (fun i ->
      let nid = TackyCfg.BlockId i in
      match Hashtbl.find_opt g.nodes nid with
      | Some (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) -> assert false
      | Some (TackyCfg.BasicBlock _) ->
        Queue.add nid workqueue;
        annotate_block ann_table nid all_copies
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
    let old_annotation = get_block_annotation ann_table nid in
    match Hashtbl.find g.nodes nid with
    | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
    | TackyCfg.BasicBlock { succ; _ } as block ->
      let incoming_copies = meet ann_table block all_copies in
      let new_node = transfer ann_table block incoming_copies in
      Hashtbl.replace g.nodes nid new_node;
      if not (equal old_annotation (get_block_annotation ann_table nid))
      then
        List.iter
          (function
            | TackyCfg.Entry -> assert false
            | TackyCfg.Exit -> ()
            | TackyCfg.BlockId _ as sid ->
              let exists_in_queue =
                Queue.fold (fun acc x -> if x = sid then true else acc) false workqueue
              in
              if not exists_in_queue then Queue.add sid workqueue)
          succ
  in
  loop ()
;;

let remove_empty_blocks (g : TackyCfg.graph) =
  let _ =
    List.init g.basic_blocks (fun i ->
      match Hashtbl.find_opt g.nodes (TackyCfg.BlockId i) with
      | Some (TackyCfg.BasicBlock { ins = []; _ }) -> TackyCfg.remove_basic_block g i
      | _ -> ())
  in
  ()
;;

let propagate_copies (cfg : TackyCfg.graph) =
  let ann_table = Hashtbl.create (Hashtbl.length cfg.nodes) in
  find_reaching_copies ann_table cfg;
  let fl = ref false in
  Hashtbl.filter_map_inplace
    (fun _ -> function
       | (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) as v -> Some v
       | TackyCfg.BasicBlock { id; ins; pred; succ } as v ->
         operand_changed := false;
         let converted_ins = List.filter_map rewrite_instruction ins in
         if !operand_changed || List.length converted_ins != List.length ins
         then (
           fl := true;
           let ins = List.map TackyInstruction.add_annotation converted_ins in
           Some (TackyCfg.BasicBlock { id; ins; pred; succ }))
         else Some v)
    cfg.nodes;
  remove_empty_blocks cfg;
  !fl
;;
