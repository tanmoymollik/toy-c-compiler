open TackyCfg
open Tacky.Ast
open SetOp

type annotation = (instruction, unit) Hashtbl.t

let pp_annotation fmt v =
  let acc =
    Hashtbl.fold
      (fun k _ acc ->
         let ins_str = show_instruction k in
         ins_str ^ if acc = "" then acc else ", " ^ acc)
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

let meet block incoming_copies =
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { pred; _ } ->
    List.fold_left
      (fun acc pid ->
         match pid with
         | TackyCfg.Entry -> Hashtbl.create 0
         | TackyCfg.Exit -> assert false
         | TackyCfg.BlockId _ ->
           let pred_out_copies = get_block_annotation pid in
           intersect acc pred_out_copies)
      incoming_copies
      pred
;;

let is_static_value = function
  | Constant _ -> false
  | Var iden -> AsmSymbolMap.is_static_var iden
;;

let update_copies reaching_copies ins =
  let default_keep dst = function
    | Copy { src = tsrc; dst = tdst } -> if tsrc = dst || tdst = dst then false else true
    | _ -> assert false
  in
  let update_copies_inner dst copies keep =
    Hashtbl.filter_map_inplace (fun k _ -> if keep dst k then Some () else None) copies
  in
  match ins with
  | Copy { src; dst } as v ->
    let exists_rev =
      match Hashtbl.find_opt reaching_copies (Copy { src = dst; dst = src }) with
      | Some _ -> true
      | None -> false
    in
    if exists_rev
    then ()
    else (
      update_copies_inner dst reaching_copies default_keep;
      Hashtbl.replace reaching_copies v ())
  | FunCall { dst; _ } ->
    let keep dst = function
      | Copy { src = tsrc; dst = tdst } ->
        if is_static_value tsrc || is_static_value tdst || tsrc = dst || tdst = dst
        then false
        else true
      | _ -> assert false
    in
    update_copies_inner dst reaching_copies keep
  | Unary { dst; _ } -> update_copies_inner dst reaching_copies default_keep
  | Binary { dst; _ } -> update_copies_inner dst reaching_copies default_keep
  | _ -> ()
;;

let transfer block reaching_copies =
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { id; ins; _ } ->
    let ins_annot =
      List.map
        (fun ins ->
           let ret = Hashtbl.copy reaching_copies in
           update_copies reaching_copies ins;
           ret)
        ins
    in
    annotate_block id (Hashtbl.copy reaching_copies);
    annotate_instructions id ins_annot
;;

let find_all_copy_ins (g : TackyCfg.graph) =
  let all_copies = Hashtbl.create 50 in
  Hashtbl.iter
    (fun _ -> function
       | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> ()
       | TackyCfg.BasicBlock { ins; _ } ->
         List.iter
           (function
             | Copy _ as v -> Hashtbl.replace all_copies v ()
             | _ -> ())
           ins)
    g.nodes;
  all_copies
;;

let find_reaching_copies (g : TackyCfg.graph) =
  let all_copies = find_all_copy_ins g in
  let workqueue = Queue.create () in
  let _ =
    List.init g.basic_blocks (fun i ->
      let nid = TackyCfg.BlockId i in
      match Hashtbl.find_opt g.nodes nid with
      | Some (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) -> assert false
      | Some (TackyCfg.BasicBlock _) ->
        Queue.add nid workqueue;
        annotate_block nid (Hashtbl.copy all_copies)
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
    | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
    | TackyCfg.BasicBlock { succ; _ } as block ->
      let incoming_copies = meet block all_copies in
      transfer block incoming_copies;
      if not (equal old_annotation (get_block_annotation nid))
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

let operand_changed = ref false

let replace_operand reaching_copies op =
  match op with
  | Constant _ -> op
  | Var _ ->
    Hashtbl.fold
      (fun k _ acc ->
         match k with
         | Copy { src; dst } ->
           if src <> op && dst = op
           then (
             operand_changed := true;
             src)
           else acc
         | _ -> assert false)
      reaching_copies
      op
;;

let rewrite_instruction ins reaching_copies =
  match ins with
  | Ret r ->
    let ret = Ret (replace_operand reaching_copies r) in
    Some ret
  | Unary { uop; src; dst } ->
    let ret = Unary { uop; src = replace_operand reaching_copies src; dst } in
    Some ret
  | Binary { bop; src1; src2; dst } ->
    let ret =
      Binary
        { bop
        ; src1 = replace_operand reaching_copies src1
        ; src2 = replace_operand reaching_copies src2
        ; dst
        }
    in
    Some ret
  | Copy { src; dst } ->
    let new_instr = Some (Copy { src = replace_operand reaching_copies src; dst }) in
    Hashtbl.fold
      (fun k _ acc ->
         match k with
         | Copy { src = tsrc; dst = tdst } ->
           if ins = k || (tsrc = dst && tdst = src) then None else acc
         | _ -> assert false)
      reaching_copies
      new_instr
  | JumpIfZero (cnd, iden) ->
    let ret = JumpIfZero (replace_operand reaching_copies cnd, iden) in
    Some ret
  | JumpIfNotZero (cnd, iden) ->
    let ret = JumpIfNotZero (replace_operand reaching_copies cnd, iden) in
    Some ret
  | FunCall { name; args; dst } ->
    let ret =
      let args = List.map (replace_operand reaching_copies) args in
      FunCall { name; args; dst }
    in
    Some ret
  | (Jump _ | Label _) as x -> Some x
  | _ -> assert false
;;

let propagate_copies (cfg : TackyCfg.graph) =
  block_ann_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  block_ins_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  find_reaching_copies cfg;
  let fl = ref false in
  Hashtbl.filter_map_inplace
    (fun _ -> function
       | (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) as v -> Some v
       | TackyCfg.BasicBlock { id; ins; pred; succ } as v ->
         operand_changed := false;
         let reaching_copies_list = get_instructions_annotation id in
         let converted_ins = List.map2 rewrite_instruction ins reaching_copies_list in
         let converted_ins = List.filter_map (fun x -> x) converted_ins in
         if !operand_changed || List.length converted_ins <> List.length ins
         then (
           fl := true;
           Some (TackyCfg.BasicBlock { id; ins = converted_ins; pred; succ }))
         else Some v)
    cfg.nodes;
  TackyCfg.remove_empty_blocks cfg;
  !fl
;;
