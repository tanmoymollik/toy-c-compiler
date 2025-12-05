type asm_symbol_entry =
  | ObjEntry of
      { tp : X64_ast.asm_type
      ; is_static : bool
      }
  | FunEntry of { defined : bool }

let asm_symbol_map : (string, asm_symbol_entry) Hashtbl.t = Hashtbl.create 100

let convert_symbols_to_asm () =
  Hashtbl.iter
    (fun iden Core.{ tp; attrs } ->
       let is_static =
         match attrs with
         | Core.StaticAttr _ -> true
         | _ -> false
       in
       match attrs with
       | Core.StaticAttr _ | Core.LocalAttr ->
         let asm_t =
           match tp with
           | C_ast.Int | C_ast.UInt -> X64_ast.DWord
           | C_ast.Long | C_ast.ULong -> X64_ast.QWord
           | C_ast.FunType _ -> assert false
         in
         Hashtbl.replace asm_symbol_map iden (ObjEntry { tp = asm_t; is_static })
       | Core.FunAttr { defined; _ } ->
         Hashtbl.replace asm_symbol_map iden (FunEntry { defined }))
    Core.symbol_map
;;
