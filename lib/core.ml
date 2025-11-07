let tmp_var_count = ref 0

let get_var_count () =
  let c = !tmp_var_count in
  tmp_var_count := c + 1;
  c
;;

let label_count = ref 0
let binary_label = "bin"
let conditional_label = "cnd"
let if_label = "if"
let loop_label = "loop"
let break_label scope = "break@" ^ scope
let continue_label scope = "continue@" ^ scope
let switch_label = "switch"
let case_label id scope = "case" ^ string_of_int id ^ "@" ^ scope
let default_label scope = "default@" ^ scope

(* Adds a numerical suffix to create a unique label. *)
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
