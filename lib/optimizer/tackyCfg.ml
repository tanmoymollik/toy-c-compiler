open Tacky.Ast

module TackyInstruction = struct
  type elm = instruction [@@deriving show]

  let convert = function
    | Ret _ -> Cfg.Return
    | Jump tgt -> Cfg.Jump tgt
    | JumpIfZero (_, tgt) | JumpIfNotZero (_, tgt) -> Cfg.CondJump tgt
    | Label tgt -> Cfg.Label tgt
    | _ -> Cfg.Other
  ;;
end

module TackyCfg = Cfg.MakeCfg (TackyInstruction)
