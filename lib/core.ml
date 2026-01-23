let tmp_iden_count = ref 0

let get_iden_count () =
  let c = !tmp_iden_count in
  tmp_iden_count := c + 1;
  c
;;

let make_unique_iden iden =
  let c = get_iden_count () in
  Printf.sprintf "%s.%d" iden c
;;

let lbl_sep = ref "@"
let num_sep = ref "#"
let label_count = ref 0
let binary_label = "bin"
let conditional_label = "cnd"
let else_if_label () = "else" ^ !lbl_sep ^ "if"
let end_if_label () = "end" ^ !lbl_sep ^ "if"
let loop_label = "loop"
let start_loop_label lbl = "start" ^ !lbl_sep ^ lbl
let break_label scope = "break" ^ !lbl_sep ^ scope
let continue_label scope = "continue" ^ !lbl_sep ^ scope
let switch_label = "switch"
let case_label id scope = "case" ^ string_of_int id ^ !lbl_sep ^ scope
let default_label scope = "default" ^ !lbl_sep ^ scope
let goto_label lbl fun_name = lbl ^ !lbl_sep ^ fun_name
let static_const_label = "static_const"
let nan_cmp_label = "nan_cmp"
let nan_cmp_end_label = "nan_cmp_end"

(* Adds a numerical suffix to create a unique label. *)
let make_unique_label prefix =
  let c = !label_count in
  label_count := c + 1;
  Printf.sprintf "%s%s%d" prefix !num_sep c
;;
