open Common

(* Maps static string literals to labels. *)
let static_consts : (string, string) Hashtbl.t = Hashtbl.create 10

let get_const_string_label str =
  match Hashtbl.find_opt static_consts str with
  | Some lbl -> lbl
  | None ->
    let lbl = Core.make_unique_label Core.static_const_label in
    Hashtbl.replace static_consts str lbl;
    let str_init = StringInit { str; null_terminated = true } in
    Hashtbl.replace
      SymbolMap.symbol_map
      lbl
      { tp = CArray (Char, String.length str + 1); attrs = ConstantAttr str_init };
    lbl
;;
