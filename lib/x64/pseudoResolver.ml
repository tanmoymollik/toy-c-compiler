open Ast
open AsmUtils

let resolve_operand fun_name register_map = function
  | Pseudo name ->
    if AsmSymbolMap.is_static_var name
    then Data name
    else (
      match Hashtbl.find_opt register_map name with
      | Some r -> Reg r
      | None ->
        let addr = get_stack_address (fun_name, name) in
        Memory (Bp, addr))
  | PseudoMem (name, offset) ->
    let addr = get_stack_address (fun_name, name) in
    if AsmSymbolMap.is_static_var name then Data name else Memory (Bp, addr - offset)
  | x -> x
;;

let resolve_instruction fun_name register_map = function
  | Mov { src; dst; tp } ->
    Mov
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; tp
      }
  | Movsx { src; dst; src_tp; dst_tp } ->
    Movsx
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; src_tp
      ; dst_tp
      }
  | MovZeroExtend { src; dst; src_tp; dst_tp } ->
    MovZeroExtend
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; src_tp
      ; dst_tp
      }
  | Lea { src; dst } ->
    Lea
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      }
  | Cvttsd2si { src; dst; dst_tp } ->
    Cvttsd2si
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; dst_tp
      }
  | Cvtsi2sd { src; dst; src_tp } ->
    Cvtsi2sd
      { src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; src_tp
      }
  | Unary (uop, src, tp) -> Unary (uop, resolve_operand fun_name register_map src, tp)
  | Binary { bop; src; dst; tp } ->
    Binary
      { bop
      ; src = resolve_operand fun_name register_map src
      ; dst = resolve_operand fun_name register_map dst
      ; tp
      }
  | Cmp { lhs; rhs; tp } ->
    Cmp
      { lhs = resolve_operand fun_name register_map lhs
      ; rhs = resolve_operand fun_name register_map rhs
      ; tp
      }
  | Idiv (src, tp) -> Idiv (resolve_operand fun_name register_map src, tp)
  | Div (src, tp) -> Div (resolve_operand fun_name register_map src, tp)
  | SetC (cnd, dst) -> SetC (cnd, resolve_operand fun_name register_map dst)
  | Push x -> Push (resolve_operand fun_name register_map x)
  | x -> x
;;

let remove_redundant_mov = function
  | Mov { src; dst; _ } as ret -> if src = dst then None else Some ret
  | ret -> Some ret
;;

let resolve_instructions fun_name body register_map =
  let body = List.map (resolve_instruction fun_name register_map) body in
  let body = List.filter_map remove_redundant_mov body in
  body
;;
