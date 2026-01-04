open TackyCfg
open Tacky.Ast
open AnnotCommon

type annotation = (string, unit) Hashtbl.t

let pp_annotation fmt v =
  let acc = Hashtbl.fold (fun k _ acc -> k ^ if acc = "" then acc else ", " ^ acc) v "" in
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

let meet block all_static_vars =
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { succ; _ } ->
    List.fold_left
      (fun acc sid ->
         match sid with
         | TackyCfg.Entry -> assert false
         | TackyCfg.Exit -> union acc all_static_vars
         | TackyCfg.BlockId _ ->
           let succ_live_vars = get_block_annotation sid in
           union acc succ_live_vars)
      (Hashtbl.create 0)
      succ
;;

let update_vars live_vars all_static_vars ins =
  let add = function
    | Constant _ -> ()
    | Var (Identifier name) -> Hashtbl.replace live_vars name ()
  in
  let remove = function
    | Constant _ -> ()
    | Var (Identifier name) -> Hashtbl.remove live_vars name
  in
  match ins with
  | Ret src -> add src
  | Unary { src; dst; _ } ->
    remove dst;
    add src
  | Binary { src1; src2; dst; _ } ->
    remove dst;
    add src1;
    add src2
  | Copy { src; dst } ->
    remove dst;
    add src
  | JumpIfZero (cnd, _) | JumpIfNotZero (cnd, _) -> add cnd
  | FunCall { args; dst; _ } ->
    remove dst;
    List.iter (fun v -> add v) args;
    Hashtbl.iter (fun k _ -> add (Var (Identifier k))) all_static_vars
  | _ -> ()
;;

let transfer block live_vars all_static_vars =
  match block with
  | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
  | TackyCfg.BasicBlock { id; ins; _ } ->
    let ins_annot =
      List.rev_map
        (fun ins ->
           let ret = Hashtbl.copy live_vars in
           update_vars live_vars all_static_vars ins;
           ret)
        (List.rev ins)
    in
    annotate_block id (Hashtbl.copy live_vars);
    annotate_instructions id ins_annot
;;

let find_reaching_vars (g : TackyCfg.graph) =
  let static_vars_list = AsmSymbolMap.static_vars () in
  let all_static_vars = Hashtbl.create (List.length static_vars_list) in
  List.iter (fun x -> Hashtbl.replace all_static_vars x ()) static_vars_list;
  let workqueue = Queue.create () in
  let _ =
    List.init g.basic_blocks (fun i ->
      (* loop in reverse order. *)
      let i = g.basic_blocks - i - 1 in
      let nid = TackyCfg.BlockId i in
      match Hashtbl.find_opt g.nodes nid with
      | Some (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) -> assert false
      | Some (TackyCfg.BasicBlock _) ->
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
    | TackyCfg.EntryNode _ | TackyCfg.ExitNode _ -> assert false
    | TackyCfg.BasicBlock { pred; _ } as block ->
      let end_live_vars = meet block all_static_vars in
      transfer block end_live_vars all_static_vars;
      if not (equal old_annotation (get_block_annotation nid))
      then
        List.iter
          (function
            | TackyCfg.Entry -> ()
            | TackyCfg.Exit -> assert false
            | TackyCfg.BlockId _ as pid ->
              let exists_in_queue =
                Queue.fold (fun acc x -> if x = pid then true else acc) false workqueue
              in
              if not exists_in_queue then Queue.add pid workqueue)
          pred
  in
  loop ()
;;

let rewrite_instruction ins live_vars =
  match ins with
  | Unary { dst; _ } | Binary { dst; _ } | Copy { dst; _ } ->
    (match dst with
     | Constant _ -> Some ins
     | Var (Identifier name) ->
       (match Hashtbl.find_opt live_vars name with
        | Some _ -> Some ins
        | None -> None))
  | FunCall _ -> Some ins
  | _ -> Some ins
;;

let eliminate_dead_stores (cfg : TackyCfg.graph) =
  (* print_endline "\n\n------------------Begin------------------";
  print_endline (TackyCfg.show_graph cfg); *)
  block_ann_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  block_ins_table := Hashtbl.create (Hashtbl.length cfg.nodes);
  find_reaching_vars cfg;
  let fl = ref false in
  Hashtbl.filter_map_inplace
    (fun _ -> function
       | (TackyCfg.EntryNode _ | TackyCfg.ExitNode _) as v -> Some v
       | TackyCfg.BasicBlock { id; ins; pred; succ } as v ->
         let live_vars_list = get_instructions_annotation id in
         let converted_ins = List.map2 rewrite_instruction ins live_vars_list in
         let converted_ins = List.filter_map (fun x -> x) converted_ins in
         if List.length converted_ins <> List.length ins
         then (
           fl := true;
           Some (TackyCfg.BasicBlock { id; ins = converted_ins; pred; succ }))
         else Some v)
    cfg.nodes;
  TackyCfg.remove_empty_blocks cfg;
  (* print_endline "\n\n------------------End------------------";
  print_endline (TackyCfg.show_graph cfg); *)
  !fl
;;
