let tmp_var_count = ref 0

let get_var_count () =
  let c = !tmp_var_count in
  tmp_var_count := c + 1;
  c
;;

let label_count = ref 0

let make_unique_label prefix =
  let c = !label_count in
  label_count := c + 1;
  Printf.sprintf "%s#%d" prefix c
;;

let label_map : (string, bool) Hashtbl.t = Hashtbl.create 100

let exists_label label =
  match Hashtbl.find_opt label_map label with
  | Some _ -> true
  | None -> false
;;

let add_label label = Hashtbl.add label_map label true
