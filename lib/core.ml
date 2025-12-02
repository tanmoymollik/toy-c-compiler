exception ParserError

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
let goto_label lbl fun_name = lbl ^ "@" ^ fun_name

(* Adds a numerical suffix to create a unique label. *)
let make_unique_label prefix =
  let c = !label_count in
  label_count := c + 1;
  Printf.sprintf "%s#%d" prefix c
;;

type static_init =
  | IntInit of int32
  | LongInit of int64
[@@deriving show]

type initial_value =
  | Tentative
  | Initial of static_init
  | NoInitial
[@@deriving show]

type identifier_attrs =
  | FunAttr of
      { defined : bool
      ; global : bool
      }
  | StaticAttr of
      { init : initial_value
      ; global : bool
      }
  | LocalAttr
[@@deriving show]

type symbol_info =
  { tp : C_ast.c_type
  ; attrs : identifier_attrs
  }
[@@deriving show]

type symbol_map_type = (string, symbol_info) Hashtbl.t

let symbol_map : symbol_map_type = Hashtbl.create 100
