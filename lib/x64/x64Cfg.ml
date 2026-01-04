open Ast

module X64Instruction = struct
  type elm = instruction [@@deriving show]

  let convert = function
    | Ret -> Cfg.Return
    | Jmp tgt -> Cfg.Jump tgt
    | JmpC (_, tgt) | JmpP tgt -> Cfg.CondJump tgt
    | Label tgt -> Cfg.Label tgt
    | _ -> Cfg.Other
  ;;
end

module X64Cfg = Cfg.MakeCfg (X64Instruction)
