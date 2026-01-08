let init_disjoint_sets () = Hashtbl.create 100
let union reg_map x y = Hashtbl.replace reg_map x y

let rec find reg_map r =
  match Hashtbl.find_opt reg_map r with
  | Some res ->
    let p = find reg_map res in
    Hashtbl.replace reg_map r p;
    p
  | None -> r
;;

let nothing_was_coalesced reg_map = Hashtbl.length reg_map = 0
