open Stdint
open Ast
open Common

let indent = String.make 4 ' '

let emit_platform_name = function
  | Identifier name -> name
;;

let data_section = Stack.create ()
let bss_section = Stack.create ()
let text_section = Stack.create ()

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
  | Shr -> "shr"
;;

let emit_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | L -> "l"
  | LE -> "le"
  | G -> "g"
  | GE -> "ge"
  | A -> "a"
  | AE -> "ae"
  | B -> "b"
  | BE -> "be"
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
  | Sp -> "sp"
;;

let emit_reg r = function
  | Byte ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx | Di | Si -> name ^ "l"
     | R8 | R9 | R10 | R11 -> name ^ "b"
     | Sp -> assert false)
  | Word ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> name ^ "x"
     | Di | Si | Sp -> name
     | R8 | R9 | R10 | R11 -> name ^ "w")
  | DWord ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> "e" ^ name ^ "x"
     | Di | Si | Sp -> "e" ^ name
     | R8 | R9 | R10 | R11 -> name ^ "d")
  | QWord ->
    let name = emit_reg_name r in
    (match r with
     | Ax | Cx | Dx -> "r" ^ name ^ "x"
     | Di | Si | Sp -> "r" ^ name
     | R8 | R9 | R10 | R11 -> name)
;;

let emit_operand_size = function
  | Byte -> "byte"
  | Word -> "word"
  | DWord -> "dword"
  | QWord -> "qword"
;;

let emit_operand = function
  | Imm i, sz -> Printf.sprintf "%s %s" (emit_operand_size sz) (Uint64.to_string i)
  | Reg r, sz -> emit_reg r sz
  | Stack i, sz ->
    if i >= 0
    then Printf.sprintf "%s [rbp - %d]" (emit_operand_size sz) i
    else Printf.sprintf "%s [rbp + %d]" (emit_operand_size sz) (-i)
  | Data iden, sz ->
    let platform_name = emit_platform_name iden in
    Printf.sprintf "%s [rel %s]" (emit_operand_size sz) platform_name
;;

let emit_instruction = function
  | Mov { src; dst; sz } ->
    Printf.sprintf "%smov %s, %s" indent (emit_operand (dst, sz)) (emit_operand (src, sz))
  | Movsx { src; dst } ->
    Printf.sprintf
      "%smovsx %s, %s"
      indent
      (emit_operand (dst, QWord))
      (emit_operand (src, DWord))
  | MovZeroExtend _ -> assert false
  | Unary (uop, src, sz) ->
    Printf.sprintf "%s%s %s" indent (emit_uop uop) (emit_operand (src, sz))
  | Binary { bop; src; dst; sz } ->
    let src_sz =
      match bop with
      | Sal | Sar | Shr -> Byte
      | _ -> sz
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_bop bop)
      (emit_operand (dst, sz))
      (emit_operand (src, src_sz))
  | Cmp { lhs; rhs; sz } ->
    Printf.sprintf "%scmp %s, %s" indent (emit_operand (lhs, sz)) (emit_operand (rhs, sz))
  | Idiv (operand, sz) -> Printf.sprintf "%sidiv %s" indent (emit_operand (operand, sz))
  | Div (operand, sz) -> Printf.sprintf "%sdiv %s" indent (emit_operand (operand, sz))
  | Cdq sz ->
    let cmd =
      match sz with
      | DWord -> "cdq"
      | QWord -> "cqo"
      | _ -> assert false
    in
    Printf.sprintf "%s%s" indent cmd
  | Jmp iden -> Printf.sprintf "%sjmp .L%s" indent (emit_identifier iden)
  | JmpC (cc, iden) ->
    Printf.sprintf "%sj%s .L%s" indent (emit_cond_code cc) (emit_identifier iden)
  | SetC (cc, operand) ->
    Printf.sprintf "%sset%s %s" indent (emit_cond_code cc) (emit_operand (operand, Byte))
  | Label iden -> Printf.sprintf ".L%s:" (emit_identifier iden)
  | Push operand -> Printf.sprintf "%spush %s" indent (emit_operand (operand, QWord))
  | Call name ->
    let platform_name = emit_platform_name name in
    let name =
      if not (Symbol_map.is_fun_defined name)
      then platform_name ^ " wrt ..plt"
      else platform_name
    in
    Printf.sprintf "%scall %s" indent name
  | Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let emit_top_level = function
  | Function { name; global; body } ->
    let name = emit_platform_name name in
    let emit_function_prologue =
      Printf.sprintf "%spush rbp\n%smov rbp, rsp\n" indent indent
    in
    let entry =
      (if global then "global " ^ name ^ "\n" else "")
      ^ (name ^ ":\n")
      ^ emit_function_prologue
      ^ (List.map emit_instruction body |> String.concat "\n")
    in
    Stack.push entry text_section
  | StaticVar { name; global; init } ->
    let name = emit_platform_name name in
    let size, specifier, value =
      match init with
      | ConstInt i -> 4, "d", Int32.to_string i
      | ConstUInt ui -> 4, "d", Uint32.to_string ui
      | ConstLong l -> 8, "q", Int64.to_string l
      | ConstULong ul -> 8, "q", Uint64.to_string ul
    in
    let is_zero =
      match init with
      | ConstInt 0l -> true
      | ConstUInt ui -> ui = Uint32.zero
      | ConstLong 0L -> true
      | ConstULong ul -> ul = Uint64.zero
      | _ -> false
    in
    let decl =
      if is_zero
      then Printf.sprintf "%s%s res%s 1" indent name specifier
      else Printf.sprintf "%s%s d%s %s" indent name specifier value
    in
    let align = if is_zero then "alignb" else "align" in
    let entry =
      Printf.sprintf "%s%s %d\n" indent align size
      ^ (if global then indent ^ "global " ^ name ^ "\n" else "")
      ^ decl
    in
    if is_zero then Stack.push entry bss_section else Stack.push entry data_section
;;

let emit_program = function
  | Program tns ->
    List.iter emit_top_level tns;
    let extern_decls = Symbol_map.extern_decls () in
    let extern_decls = List.map (fun s -> "extern " ^ s) extern_decls in
    let f acc e = e :: acc in
    let data_body = Stack.fold f [] data_section in
    let bss_body = Stack.fold f [] bss_section in
    let text_body = Stack.fold f [] text_section in
    "section .text\n"
    ^ String.concat "\n" extern_decls
    ^ (if List.length extern_decls > 0 then "\n\n" else "\n")
    ^ String.concat "\n\n" text_body
    ^ (if List.length text_body > 0 then "\n\n" else "\n")
    ^ "section .data\n"
    ^ String.concat "\n\n" data_body
    ^ (if List.length data_body > 0 then "\n\n" else "\n")
    ^ "section .bss\n"
    ^ String.concat "\n\n" bss_body
;;
