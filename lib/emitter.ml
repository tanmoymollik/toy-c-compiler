open X64_ast

let indent = String.make 4 ' '
let platform = ref Platform.Mac
let emit_function_name name = if !platform = Platform.Linux then name else "_" ^ name
let extern_funcs : (string, bool) Hashtbl.t = Hashtbl.create 10

let emit_identifier = function
  | Identifier iden -> iden
;;

let emit_uop = function
  | Neg -> "neg"
  | Not -> "not"
;;

let emit_bop = function
  | Add -> "add"
  | Sub -> "sub"
  | Imul -> "imul"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Sal -> "sal"
  | Sar -> "sar"
;;

let emit_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | LE -> "le"
  | G -> "g"
  | GE -> "ge"
;;

let emit_reg_name = function
  | Ax -> "a"
  | Cx -> "c"
  | Dx -> "d"
  | Di -> "di"
  | Si -> "si"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
;;

let emit_reg r = function
  | Byte ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx | Di | Si -> name ^ "l"
     | R8 | R9 | R10 | R11 -> name ^ "b")
  | Word ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> name ^ "x"
     | Di | Si -> name
     | R8 | R9 | R10 | R11 -> name ^ "w")
  | DWord ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> "e" ^ name ^ "x"
     | Di | Si -> "e" ^ name
     | R8 | R9 | R10 | R11 -> name ^ "d")
  | QWord ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> "r" ^ name ^ "x"
     | Di | Si -> "r" ^ name
     | R8 | R9 | R10 | R11 -> name)
;;

let emit_operand_size = function
  | Byte -> "byte"
  | Word -> "word"
  | DWord -> "dword"
  | QWord -> "qword"
;;

let emit_operand = function
  | Imm i, sz -> Printf.sprintf "%s %d" (emit_operand_size sz) i
  | Reg r, sz -> emit_reg r sz
  | Stack i, sz ->
    if i >= 0
    then Printf.sprintf "%s [rbp - %d]" (emit_operand_size sz) i
    else Printf.sprintf "%s [rbp + %d]" (emit_operand_size sz) (-i)
;;

let emit_instruction = function
  | Mov { src; dst } ->
    Printf.sprintf "%smov %s, %s" indent (emit_operand dst) (emit_operand src)
  | Unary (uop, src) -> Printf.sprintf "%s%s %s" indent (emit_uop uop) (emit_operand src)
  | Binary { bop; src; dst } ->
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_bop bop)
      (emit_operand dst)
      (emit_operand src)
  | Cmp { lhs; rhs } ->
    Printf.sprintf "%scmp %s, %s" indent (emit_operand lhs) (emit_operand rhs)
  | Idiv operand -> Printf.sprintf "%sidiv %s" indent (emit_operand operand)
  | Cdq -> Printf.sprintf "%scdq" indent
  | Jmp iden -> Printf.sprintf "%sjmp .L%s" indent (emit_identifier iden)
  | JmpC (cc, iden) ->
    Printf.sprintf "%sj%s .L%s" indent (emit_cond_code cc) (emit_identifier iden)
  | SetC (cc, (operand_type, _)) ->
    Printf.sprintf
      "%sset%s %s"
      indent
      (emit_cond_code cc)
      (emit_operand (operand_type, Byte))
  | Label iden -> Printf.sprintf ".L%s:" (emit_identifier iden)
  | AllocStack i -> Printf.sprintf "%ssub rsp, %d" indent i
  | DeallocStack i -> Printf.sprintf "%sadd rsp, %d" indent i
  | Push operand -> Printf.sprintf "%spush %s" indent (emit_operand operand)
  | Call (X64_ast.Identifier name) ->
    let platform_name = emit_function_name name in
    let name =
      match Hashtbl.find_opt Core.symbol_map name with
      | None | Some Core.{ defined = false; _ } ->
        Hashtbl.add extern_funcs platform_name true;
        platform_name ^ " wrt ..plt"
      | _ -> platform_name
    in
    Printf.sprintf "%scall %s" indent name
  | Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let emit_function_prologue = Printf.sprintf "%spush rbp\n%smov rbp, rsp\n" indent indent

let emit_function_def = function
  | Function { name = Identifier name; body } ->
    let name = emit_function_name name in
    Printf.sprintf "global %s\n%s:\n" name name
    ^ emit_function_prologue
    ^ (List.map emit_instruction body |> String.concat "\n")
;;

let emit_program plt = function
  | Program fns ->
    platform := plt;
    let func_bodies = List.map emit_function_def fns in
    let f k _ acc = ("extern " ^ k) :: acc in
    let extern_decls = Hashtbl.fold f extern_funcs [] in
    let prog_epilogue =
      if !platform = Platform.Linux
      then ""
      (* then "\n\nsection .note.GNU-stack,\"\",@progbits\n" *)
      else ""
    in
    "section .text\n"
    ^ String.concat "\n" extern_decls
    ^ "\n\n"
    ^ String.concat "\n\n" func_bodies
    ^ prog_epilogue
;;
