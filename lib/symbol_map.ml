open Common

type initial_value =
  | Tentative
  | Initial of const
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

let fold f acc =
  Hashtbl.fold
    (fun iden { tp; attrs } acc ->
       match attrs with
       | StaticAttr { init; global } ->
         let name = Identifier iden in
         (match init with
          | Initial i -> f name global tp i :: acc
          | Tentative ->
            let init = c_type_zero tp in
            f name global tp init :: acc
          | NoInitial -> acc)
       | _ -> acc)
    symbol_map
    acc
;;
