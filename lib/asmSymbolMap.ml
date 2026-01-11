open Common

type asm_symbol_info =
  | ObjInfo of
      { tp : asm_type
      ; is_static : bool
      ; is_signed : bool
      ; is_extern : bool
      }
  | FunInfo of
      { defined : bool
      ; ret : asm_type
      }
[@@deriving show]

let asm_symbol_map : (string, asm_symbol_info) Hashtbl.t = Hashtbl.create 100

let add_obj_info iden tp is_static is_extern =
  let asm_tp = get_asm_type_for_c_type tp in
  let is_signed = signed_c_type tp in
  Hashtbl.replace
    asm_symbol_map
    iden
    (ObjInfo { tp = asm_tp; is_static; is_signed; is_extern })
;;

let gen_asm_symbol_map () =
  Hashtbl.iter
    (fun iden SymbolMap.{ tp; attrs } ->
       match attrs with
       | StaticAttr { init; _ } ->
         let is_extern =
           match init with
           | NoInitial -> true
           | _ -> false
         in
         add_obj_info iden tp true is_extern
       | ConstantAttr init ->
         (match init with
          | StringInit _ -> add_obj_info iden tp true false
          | _ -> assert false)
       | FunAttr { defined; _ } ->
         let ret =
           match tp with
           | FunType { ret; _ } -> get_asm_type_for_c_type ret
           | _ -> assert false
         in
         Hashtbl.replace asm_symbol_map iden (FunInfo { defined; ret })
       | LocalAttr -> add_obj_info iden tp false false)
    SymbolMap.symbol_map
;;

(* Can only be called on vars. *)
let is_signed_var = function
  | Identifier iden ->
    (match Hashtbl.find_opt asm_symbol_map iden with
     | Some (ObjInfo { is_signed; _ }) -> is_signed
     | _ -> assert false)
;;

(* Can only be called on vars. *)
let is_static_var = function
  | Identifier iden ->
    (match Hashtbl.find_opt asm_symbol_map iden with
     | Some (ObjInfo { is_static; _ }) -> is_static
     | _ -> assert false)
;;

(* Can only be called on vars. *)
let get_var_type = function
  | Identifier iden ->
    (match Hashtbl.find_opt asm_symbol_map iden with
     | Some (ObjInfo { tp; _ }) -> tp
     | _ -> assert false)
;;

(* Can only be called on function. *)
let is_fun_defined = function
  | Identifier iden ->
    (match Hashtbl.find_opt asm_symbol_map iden with
     | Some (FunInfo { defined; _ }) -> defined
     | _ -> assert false)
;;

(* Can only be called on function. *)
let get_fun_ret_type = function
  | Identifier iden ->
    (match Hashtbl.find_opt asm_symbol_map iden with
     | Some (FunInfo { ret; _ }) -> ret
     | _ -> assert false)
;;

let extern_decls () =
  Hashtbl.fold
    (fun iden info acc ->
       match info with
       | ObjInfo { is_extern = true; _ } -> iden :: acc
       | FunInfo { defined = false; _ } -> iden :: acc
       | _ -> acc)
    asm_symbol_map
    []
;;

let static_vars () =
  Hashtbl.fold
    (fun iden info acc ->
       match info with
       | ObjInfo { is_static = true; _ } -> iden :: acc
       | _ -> acc)
    asm_symbol_map
    []
;;
