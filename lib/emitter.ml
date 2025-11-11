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

(* TODO: Rename to emit_reg32. *)
let emit_reg = function
  | X64_ast.Ax -> "eax"
  | X64_ast.Dx -> "edx"
  | X64_ast.Cx -> "ecx"
  | X64_ast.Cl -> "cl"
  | X64_ast.R10 -> "r10d"
  | X64_ast.R11 -> "r11d"
;;

let emit_reg8 = function
  | X64_ast.Ax -> "al"
  | X64_ast.Dx -> "dl"
  | X64_ast.Cx -> "cl"
  | X64_ast.Cl -> "cl"
  | X64_ast.R10 -> "r10b"
  | X64_ast.R11 -> "r11b"
;;

let emit_operand32 = function
  | X64_ast.Imm i -> Printf.sprintf "%d" i
  | X64_ast.Reg r -> emit_reg r
  | X64_ast.Stack i -> Printf.sprintf "dword [rbp - %d]" i
;;

let emit_operand8 = function
  | X64_ast.Imm i -> Printf.sprintf "%d" i
  | X64_ast.Reg r -> emit_reg8 r
  | X64_ast.Stack i -> Printf.sprintf "byte [rbp - %d]" i
;;

let emit_instruction = function
  | X64_ast.Mov { src; dst } ->
    Printf.sprintf "%smov %s, %s" indent (emit_operand32 dst) (emit_operand32 src)
  | X64_ast.Unary (uop, src) ->
    Printf.sprintf "%s%s %s" indent (emit_uop uop) (emit_operand32 src)
  | X64_ast.Binary { bop; src; dst } ->
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_bop bop)
      (emit_operand32 dst)
      (emit_operand32 src)
  | X64_ast.Cmp { lhs; rhs } ->
    Printf.sprintf "%scmp %s, %s" indent (emit_operand32 lhs) (emit_operand32 rhs)
  | X64_ast.Idiv operand -> Printf.sprintf "%sidiv %s" indent (emit_operand32 operand)
  | X64_ast.Cdq -> Printf.sprintf "%scdq" indent
  | X64_ast.Jmp iden -> Printf.sprintf "%sjmp .L%s" indent (emit_identifier iden)
  | X64_ast.JmpC (cc, iden) ->
    Printf.sprintf "%sj%s .L%s" indent (emit_cond_code cc) (emit_identifier iden)
  | X64_ast.SetC (cc, operand) ->
    Printf.sprintf "%sset%s %s" indent (emit_cond_code cc) (emit_operand8 operand)
  | X64_ast.Label iden -> Printf.sprintf ".L%s:" (emit_identifier iden)
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
  | X64_ast.Program fns ->
    let f = List.hd fns in
    emit_function_def platform f
    ^
    if platform = Platform.Linux then ".section .note.GNU-stack,\"\",@progbits\n" else ""
;;
