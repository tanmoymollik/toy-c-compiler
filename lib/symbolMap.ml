open Common

type initial_value =
  | Tentative
  | Initial of static_init list
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
  | ConstantAttr of static_init
  | LocalAttr
[@@deriving show]

type symbol_info =
  { tp : c_type
  ; attrs : identifier_attrs
  }
[@@deriving show]

type symbol_map_type = (string, symbol_info) Hashtbl.t

let symbol_map : symbol_map_type = Hashtbl.create 100

let add_local_var iden vtp =
  match iden with
  | Identifier name -> Hashtbl.replace symbol_map name { tp = vtp; attrs = LocalAttr }
;;

let is_global_fun = function
  | Identifier name ->
    (match Hashtbl.find_opt symbol_map name with
     | Some { attrs = FunAttr { global; _ }; _ } -> global
     | _ -> assert false)
;;
