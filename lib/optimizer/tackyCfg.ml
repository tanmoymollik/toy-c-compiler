type annotation = (Tacky.Ast.instruction, bool) Hashtbl.t

let pp_annotation fmt v =
  let acc =
    Hashtbl.fold
      (fun k _ acc ->
         let ins_str = Tacky.Ast.show_instruction k in
         ins_str ^ if acc = "" then acc else ", ")
      v
      ""
  in
  Format.fprintf fmt "annotation:\n[\n%s\n]" acc
;;

module TackyInstruction = struct
  type elm = Tacky.Ast.instruction * annotation [@@deriving show]

  let convert = function
    | Tacky.Ast.Ret _, _ -> Cfg.Return
    | Tacky.Ast.Jump tgt, _ -> Cfg.Jump tgt
    | (Tacky.Ast.JumpIfZero (_, tgt) | Tacky.Ast.JumpIfNotZero (_, tgt)), _ ->
      Cfg.CondJump tgt
    | Tacky.Ast.Label tgt, _ -> Cfg.Label tgt
    | _ -> Cfg.Other
  ;;

  let add_annotation i = i, Hashtbl.create 5

  let strip_annotation = function
    | a, _ -> a
  ;;
end

module TackyCfg = Cfg.MakeCfg (TackyInstruction)
