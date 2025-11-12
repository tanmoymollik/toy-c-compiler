let indent = String.make 4 ' '

let emit_identifier = function
  | X64_ast.Identifier iden -> iden
;;

let emit_uop = function
  | X64_ast.Neg -> "neg"
  | X64_ast.Not -> "not"
;;

let emit_bop = function
  | X64_ast.Add -> "add"
  | X64_ast.Sub -> "sub"
  | X64_ast.Imul -> "imul"
  | X64_ast.And -> "and"
  | X64_ast.Or -> "or"
  | X64_ast.Xor -> "xor"
  | X64_ast.Sal -> "sal"
  | X64_ast.Sar -> "sar"
;;

let emit_cond_code = function
  | X64_ast.E -> "e"
  | X64_ast.NE -> "ne"
  | X64_ast.L -> "l"
  | X64_ast.LE -> "le"
  | X64_ast.G -> "g"
  | X64_ast.GE -> "ge"
;;

let emit_reg_name = function
  | X64_ast.Ax -> "a"
  | X64_ast.Cx -> "c"
  | X64_ast.Dx -> "d"
  | X64_ast.R10 -> "r10"
  | X64_ast.R11 -> "r11"
;;

(* TODO: Rename to emit_reg32. *)
let emit_reg r = function
  | X64_ast.Byte ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> name ^ "l"
     | R10 | R11 -> name ^ "b")
  | X64_ast.Word -> assert false
  | X64_ast.DWord ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> "e" ^ name ^ "x"
     | R10 | R11 -> name ^ "d")
  | X64_ast.QWord -> assert false
;;

let emit_stack_size = function
  | X64_ast.Byte -> "byte"
  | X64_ast.Word -> assert false
  | X64_ast.DWord -> "dword"
  | X64_ast.QWord -> assert false
;;

let emit_operand = function
  | X64_ast.Imm i, _ -> Printf.sprintf "%d" i
  | X64_ast.Reg r, sz -> emit_reg r sz
  | X64_ast.Stack i, sz -> Printf.sprintf "%s [rbp - %d]" (emit_stack_size sz) i
;;

let emit_instruction = function
  | X64_ast.Mov { src; dst } ->
    Printf.sprintf "%smov %s, %s" indent (emit_operand dst) (emit_operand src)
  | X64_ast.Unary (uop, src) ->
    Printf.sprintf "%s%s %s" indent (emit_uop uop) (emit_operand src)
  | X64_ast.Binary { bop; src; dst } ->
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_bop bop)
      (emit_operand dst)
      (emit_operand src)
  | X64_ast.Cmp { lhs; rhs } ->
    Printf.sprintf "%scmp %s, %s" indent (emit_operand lhs) (emit_operand rhs)
  | X64_ast.Idiv operand -> Printf.sprintf "%sidiv %s" indent (emit_operand operand)
  | X64_ast.Cdq -> Printf.sprintf "%scdq" indent
  | X64_ast.Jmp iden -> Printf.sprintf "%sjmp .L%s" indent (emit_identifier iden)
  | X64_ast.JmpC (cc, iden) ->
    Printf.sprintf "%sj%s .L%s" indent (emit_cond_code cc) (emit_identifier iden)
  | X64_ast.SetC (cc, (operand_type, _)) ->
    Printf.sprintf
      "%sset%s %s"
      indent
      (emit_cond_code cc)
      (emit_operand (operand_type, X64_ast.Byte))
  | X64_ast.Label iden -> Printf.sprintf ".L%s:" (emit_identifier iden)
  | X64_ast.AllocStack i -> Printf.sprintf "%ssub rsp, %d" indent i
  | X64_ast.Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let has_main = ref false
let emit_function_prologue = Printf.sprintf "%spush rbp\n%smov rbp, rsp\n" indent indent

let emit_function_def = function
  | X64_ast.Function { name = X64_ast.Identifier name; body } ->
    if name = "_main" then has_main := true;
    Printf.sprintf "%s:\n" name
    ^ emit_function_prologue
    ^ (List.map emit_instruction body |> String.concat "\n")
;;

let emit_program platform = function
  | X64_ast.Program fns ->
    let fn_codes = List.map emit_function_def fns in
    let prog_prologue =
      "section .text\n" ^ if !has_main then "global _main\n\n" else "\n"
    in
    let body = String.concat "\n\n" fn_codes in
    let prog_epilogue =
      if platform = Platform.Linux
      then ".section .note.GNU-stack,\"\",@progbits\n"
      else ""
    in
    prog_prologue ^ body ^ prog_epilogue
;;
