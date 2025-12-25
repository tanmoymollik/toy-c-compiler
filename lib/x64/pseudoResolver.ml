open Ast
open AsmUtils

let resolve_operand fun_name = function
  | Pseudo name ->
    let addr = get_stack_address (fun_name, name) in
    if AsmSymbolMap.is_static_var name then Data name else Memory (Bp, addr)
  | PseudoMem (name, offset) ->
    let addr = get_stack_address (fun_name, name) in
    if AsmSymbolMap.is_static_var name then Data name else Memory (Bp, addr - offset)
  | x -> x
;;

let resolve_instruction fun_name = function
  | Mov { src; dst; tp } ->
    Mov { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst; tp }
  | Movsx { src; dst } ->
    Movsx { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst }
  | MovZeroExtend { src; dst } ->
    MovZeroExtend
      { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst }
  | Lea { src; dst } ->
    Lea { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst }
  | Cvttsd2si { src; dst; dst_tp } ->
    Cvttsd2si
      { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst; dst_tp }
  | Cvtsi2sd { src; dst; src_tp } ->
    Cvtsi2sd
      { src = resolve_operand fun_name src; dst = resolve_operand fun_name dst; src_tp }
  | Unary (uop, src, tp) -> Unary (uop, resolve_operand fun_name src, tp)
  | Binary { bop; src; dst; tp } ->
    Binary
      { bop; src = resolve_operand fun_name src; dst = resolve_operand fun_name dst; tp }
  | Cmp { lhs; rhs; tp } ->
    Cmp { lhs = resolve_operand fun_name lhs; rhs = resolve_operand fun_name rhs; tp }
  | Idiv (src, tp) -> Idiv (resolve_operand fun_name src, tp)
  | Div (src, tp) -> Div (resolve_operand fun_name src, tp)
  | SetC (cnd, dst) -> SetC (cnd, resolve_operand fun_name dst)
  | Push x -> Push (resolve_operand fun_name x)
  | x -> x
;;
