open Ast
open Common
open Stdint

let indent = String.make 4 ' '

let emit_identifier = function
  | Identifier ident -> ident
;;

let emit_uop = function
  | Neg -> "neg"
  | Not -> "not"
;;

let emit_reg = function
  | A0 -> "a0"
  | T0 -> "t0"
  | T1 -> "t1"
;;

let emit_operand = function
  | Imm i -> Printf.sprintf "%s" (Uint64.to_string i)
  | Reg r -> emit_reg r
  | Stack i -> Printf.sprintf "-%d(fp)" i
;;

let emit_instruction = function
  | Mov { src; dst } ->
    Printf.sprintf "%smv %s, %s" indent (emit_operand dst) (emit_operand src)
  | Unary { uop; src; dst } ->
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_uop uop)
      (emit_operand dst)
      (emit_operand src)
  | Li { src; dst } ->
    Printf.sprintf "%sli %s, %s" indent (emit_operand dst) (emit_operand src)
  | Ld { src; dst } ->
    Printf.sprintf "%slw %s, %s" indent (emit_operand dst) (emit_operand src)
  | Sd { src; dst } ->
    Printf.sprintf "%ssw %s, %s" indent (emit_operand src) (emit_operand dst)
  | AllocStack i -> Printf.sprintf "%saddi sp, sp, -%d" indent i
  | Ret ->
    (indent ^ "mv sp, fp\n")
    ^ (indent ^ "ld ra, 8(sp)\n")
    ^ (indent ^ "ld fp, 0(sp)\n")
    ^ (indent ^ "addi sp, sp, 16\n")
    ^ Printf.sprintf "%sret\n" indent
;;

let emit_function_prologue =
  (indent ^ "addi sp, sp, -16\n")
  ^ (indent ^ "sd ra, 8(sp)\n")
  ^ (indent ^ "sd fp, 0(sp)\n")
  ^ Printf.sprintf "%smv fp, sp\n" indent
;;

let emit_function_def = function
  | Function { name = Identifier name; body } ->
    ".section .text\n"
    ^ Printf.sprintf ".global %s\n\n" name
    ^ Printf.sprintf "%s:\n" name
    ^ emit_function_prologue
    ^ (List.map emit_instruction body |> String.concat "\n")
;;

let emit_program = function
  | Program fns -> List.map emit_function_def fns |> String.concat "\n"
;;
