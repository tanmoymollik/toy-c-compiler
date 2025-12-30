module TackyInstruction = struct
  type elm = Tacky.Ast.instruction [@@deriving show]

  let convert = function
    | Tacky.Ast.Ret _ -> Cfg.Return
    | Tacky.Ast.Jump tgt -> Cfg.Jump tgt
    | Tacky.Ast.JumpIfZero (_, tgt) | Tacky.Ast.JumpIfNotZero (_, tgt) -> Cfg.CondJump tgt
    | Tacky.Ast.Label tgt -> Cfg.Label tgt
    | _ -> Cfg.Other
  ;;
end

module TackyCfg = Cfg.MakeCfg (TackyInstruction)
