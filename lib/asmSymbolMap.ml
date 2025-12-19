open Common

type asm_symbol_info =
  | ObjInfo of
      { tp : asm_type
      ; is_static : bool
      ; is_signed : bool
      ; is_extern : bool
      ; is_constant : bool
      }
  | FunInfo of
      { defined : bool
      ; ret : asm_type
      }
[@@deriving show]

let asm_symbol_map : (string, asm_symbol_info) Hashtbl.t = Hashtbl.create 100

(* Can only be called on vars. *)
let get_asm_type_for_c_type = function
  | Int | UInt -> DWord
  | Long | ULong -> QWord
  | Double -> AsmDouble
  | FunType _ -> assert false
  | Pointer _ -> assert false
;;

let gen_obj_info iden tp is_static is_extern =
  let asm_tp = get_asm_type_for_c_type tp in
  let is_signed = signed_c_type tp in
  Hashtbl.replace
    asm_symbol_map
    iden
    (ObjInfo { tp = asm_tp; is_static; is_signed; is_extern; is_constant = false })
;;

let gen_asm_symbol_map () =
  Hashtbl.iter
    (fun iden Symbol_map.{ tp; attrs } ->
       match attrs with
       | StaticAttr { init; _ } ->
         let is_extern =
           match init with
           | NoInitial -> true
           | _ -> false
         in
         gen_obj_info iden tp true is_extern
       | FunAttr { defined; _ } ->
         let ret =
           match tp with
           | FunType { ret; _ } -> get_asm_type_for_c_type ret
           | _ -> assert false
         in
         Hashtbl.replace asm_symbol_map iden (FunInfo { defined; ret })
       | LocalAttr -> gen_obj_info iden tp false false)
    Symbol_map.symbol_map
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
