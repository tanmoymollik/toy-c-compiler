open Common
open C_ast

(* Returns the size of type in bytes. *)
let rec c_type_size = function
  | Char | SChar | UChar -> 1
  | Int -> 4
  | UInt -> 4
  | Long -> 8
  | ULong -> 8
  | Double -> 8
  | Void -> assert false
  | FunType _ -> assert false
  | Pointer _ -> 8
  | CArray (tp, sz) -> sz * c_type_size tp
  | Structure tag ->
    (match Hashtbl.find_opt SymbolMap.struct_map tag with
     | Some (SymbolMap.StructEntry { size; _ }) -> size
     | None -> assert false)
;;

let is_complete = function
  | Void -> false
  | Structure tag ->
    (match Hashtbl.find_opt SymbolMap.struct_map tag with
     | Some _ -> true
     | None -> false)
  | _ -> true
;;

let is_pointer_to_complete = function
  | Pointer tp -> is_complete tp
  | _ -> false
;;

(* Returns the type both t1 and t2 should be converted to. *)
let rec get_common_type t1 t2 =
  if is_char_type t1
  then get_common_type Int t2
  else if is_char_type t2
  then get_common_type t1 Int
  else if t1 = t2
  then t1
  else if t1 = Double || t2 = Double
  then Double
  else if c_type_size t1 = c_type_size t2
  then if signed_c_type t1 then t2 else t1
  else if c_type_size t1 > c_type_size t2
  then t1
  else t2
;;

let get_asm_type_for_c_type = function
  | Char | SChar | UChar -> Byte
  | Int | UInt -> DWord
  | Long | ULong | Pointer _ -> QWord
  | Double -> AsmDouble
  | Void -> assert false
  | FunType _ -> assert false
  | CArray (tp, sz) ->
    let sz = sz * c_type_size tp in
    let alignment = if sz >= 16 then 16 else scalar_type_alignment tp in
    ByteArray { sz; alignment }
  | Structure _ -> assert false
;;

let init_zero tp = ZeroInit { bytes = c_type_size tp }

let rec zero_initializer tp =
  match tp with
  | CArray (e_tp, sz) -> CompoundInit (List.init sz (fun _ -> zero_initializer e_tp), tp)
  | Pointer _ -> SingleInit (Constant (c_type_zero tp, ULong), tp)
  | Structure tag ->
    (match Hashtbl.find_opt SymbolMap.struct_map tag with
     | Some (SymbolMap.StructEntry { members; _ }) ->
       let member_inits =
         Array.to_list members
         |> List.map (fun (SymbolMap.MemberEntry { tp; _ }) -> zero_initializer tp)
       in
       CompoundInit (member_inits, tp)
     | None -> raise (Errors.SemanticError "Incomplete structure type"))
  | _ -> SingleInit (Constant (c_type_zero tp, tp), tp)
;;
