type initial_value =
  | Tentative
  | Initial of Common.const
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

let add_local_var iden vtp =
  match iden with
  | Common.Identifier name ->
    Hashtbl.replace symbol_map name { tp = vtp; attrs = LocalAttr }
;;

let is_global_fun = function
  | Common.Identifier name ->
    (match Hashtbl.find_opt symbol_map name with
     | Some { attrs = FunAttr { global; _ }; _ } -> global
     | _ -> assert false)
;;

let fold f acc =
  Hashtbl.fold
    (fun iden { tp; attrs } acc ->
       match attrs with
       | StaticAttr { init; global } ->
         let name = Common.Identifier iden in
         (match init with
          | Initial i -> f name global i :: acc
          | Tentative ->
            let init =
              match tp with
              | C_ast.Int -> Common.ConstInt 0l
              | C_ast.UInt -> Common.ConstUInt 0i
              | C_ast.Long -> Common.ConstLong 0L
              | C_ast.ULong -> Common.ConstULong 0I
              | C_ast.FunType _ -> assert false
            in
            f name global init :: acc
          | NoInitial -> acc)
       | _ -> acc)
    symbol_map
    acc
;;

let get_var_type = function
  | Common.Identifier iden ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { tp; _ } -> tp
     | None -> assert false)
;;

let get_fun_ret_type = function
  | Common.Identifier iden ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { tp = C_ast.FunType { ret; _ }; _ } -> ret
     | _ -> assert false)
;;

let is_static_var = function
  | Common.Identifier iden ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { tp = C_ast.FunType _; _ } -> assert false
     | Some { attrs = StaticAttr _; _ } -> true
     | _ -> false)
;;

let is_signed_var = function
  | Common.Identifier iden ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { tp; _ } -> C_ast.signed tp
     | None -> assert false)
;;

let is_fun_defined = function
  | Common.Identifier iden ->
    (match Hashtbl.find_opt symbol_map iden with
     | Some { attrs = FunAttr { defined; _ }; _ } -> defined
     | _ -> assert false)
;;

let extern_decls () =
  Hashtbl.fold
    (fun iden { attrs; _ } acc ->
       match attrs with
       | StaticAttr { init = NoInitial; _ } -> iden :: acc
       | FunAttr { defined = false; _ } -> iden :: acc
       | _ -> acc)
    symbol_map
    []
;;
