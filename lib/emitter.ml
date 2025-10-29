let indent = String.make 4 ' '

let emit_identifier = function
  | X64_ast.Identifier ident -> ident
;;

let emit_uop = function
  | X64_ast.Neg -> "neg"
  | X64_ast.Not -> "not"
;;

let emit_bop = function
  | X64_ast.Add -> "add"
  | X64_ast.Sub -> "sub"
  | X64_ast.Mul -> "imul"
;;

let emit_reg = function
  | X64_ast.AX -> "eax"
  | X64_ast.DX -> "edx"
  | X64_ast.R10 -> "r10d"
  | X64_ast.R11 -> "r11d"
;;

let emit_operand = function
  | X64_ast.Imm i -> Printf.sprintf "%d" i
  | X64_ast.Reg r -> emit_reg r
  | X64_ast.Stack i -> Printf.sprintf "dword [rbp - %d]" i
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
  | X64_ast.Idiv operand -> Printf.sprintf "%sidiv %s" indent (emit_operand operand)
  | X64_ast.Cdq -> Printf.sprintf "%scdq" indent
  | X64_ast.AllocStack i -> Printf.sprintf "%ssub rsp, %d" indent i
  | X64_ast.Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let emit_function_prologue = Printf.sprintf "%spush rbp\n%smov rbp, rsp\n" indent indent

let emit_function_def platform = function
  | X64_ast.Function { name; body } ->
    let name_prefix = if platform = Platform.Linux then "" else "_" in
    let name = Printf.sprintf "%s%s" name_prefix (emit_identifier name) in
    "section .text\n"
    ^ Printf.sprintf "global %s\n\n" name
    ^ Printf.sprintf "%s:\n" name
    ^ emit_function_prologue
    ^ (List.map emit_instruction body |> String.concat "\n")
;;

let emit_program platform = function
  | X64_ast.Program f ->
    emit_function_def platform f
    ^
    if platform = Platform.Linux then ".section .note.GNU-stack,\"\",@progbits\n" else ""
;;
