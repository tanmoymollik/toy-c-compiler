let indent_size = 4

let emit_identifier = function
  | X64_ast.Identifier ident -> ident
;;

let emit_operand = function
  | X64_ast.Imm i -> Printf.sprintf "%d" i
  | X64_ast.Register -> "eax"
;;

let emit_instruction = function
  | X64_ast.Mov { src; dst } ->
    Printf.sprintf
      "%smov %s, %s"
      (String.make indent_size ' ')
      (emit_operand dst)
      (emit_operand src)
  | X64_ast.Ret -> Printf.sprintf "%sret" (String.make indent_size ' ')
;;

let emit_function_def platform = function
  | X64_ast.Function { name; body } ->
    let name_prefix = if platform = Platform.Linux then "" else "_" in
    let name = Printf.sprintf "%s%s" name_prefix (emit_identifier name) in
    "section .text\n"
    ^ Printf.sprintf "global %s\n\n" name
    ^ Printf.sprintf "%s:\n" name
    ^ (List.map emit_instruction body |> String.concat "\n")
;;

let emit_program platform = function
  | X64_ast.Program f ->
    emit_function_def platform f
    ^
    if platform = Platform.Linux then ".section .note.GNU-stack,\"\",@progbits\n" else ""
;;
