open Stdint
open Ast

let indent = String.make 4 ' '
let platform = ref Platform.Mac
let emit_platform_name name = if !platform = Platform.Linux then name else "_" ^ name
let extern_decls : (string, bool) Hashtbl.t = Hashtbl.create 10
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
    let iden = emit_identifier iden in
    let platform_name = emit_platform_name iden in
    (match Hashtbl.find_opt Core.symbol_map iden with
     | None | Some Core.{ attrs = StaticAttr { init = NoInitial; _ }; _ } ->
       Hashtbl.replace extern_decls platform_name true
     | _ -> ());
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
  | Call (Identifier name) ->
    let platform_name = emit_platform_name name in
    let name =
      match Hashtbl.find_opt Core.symbol_map name with
      | None | Some Core.{ attrs = FunAttr { defined = false; _ }; _ } ->
        Hashtbl.replace extern_decls platform_name true;
        platform_name ^ " wrt ..plt"
      | _ -> platform_name
    in
    Printf.sprintf "%scall %s" indent name
  | Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let emit_top_level = function
  | Function { name = Identifier name; global; body } ->
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
  | StaticVar { name = Identifier iden; global; init } ->
    let iden = emit_platform_name iden in
    let size, specifier, value =
      match init with
      | Core.IntInit i -> 4, "d", Int32.to_string i
      | Core.UIntInit ui -> 4, "d", Uint32.to_string ui
      | Core.LongInit l -> 8, "q", Int64.to_string l
      | Core.ULongInit ul -> 8, "q", Uint64.to_string ul
    in
    let is_zero =
      match init with
      | Core.IntInit 0l -> true
      | Core.UIntInit ui -> ui = Uint32.zero
      | Core.LongInit 0L -> true
      | Core.ULongInit ul -> ul = Uint64.zero
      | _ -> false
    in
    let decl =
      if is_zero
      then Printf.sprintf "%s%s res%s 1" indent iden specifier
      else Printf.sprintf "%s%s d%s %s" indent iden specifier value
    in
    let align = if is_zero then "alignb" else "align" in
    let entry =
      Printf.sprintf "%s%s %d\n" indent align size
      ^ (if global then indent ^ "global " ^ iden ^ "\n" else "")
      ^ decl
    in
    if is_zero then Stack.push entry bss_section else Stack.push entry data_section
;;

let emit_program plt = function
  | Program tns ->
    platform := plt;
    List.iter emit_top_level tns;
    let f k _ acc = ("extern " ^ k) :: acc in
    let extern_decls = Hashtbl.fold f extern_decls [] in
    let f acc e = e :: acc in
    let data_body = Stack.fold f [] data_section in
    let bss_body = Stack.fold f [] bss_section in
    let text_body = Stack.fold f [] text_section in
    "section .data\n"
    ^ String.concat "\n\n" data_body
    ^ (if List.length data_body > 0 then "\n\n" else "\n")
    ^ "section .bss\n"
    ^ String.concat "\n\n" bss_body
    ^ (if List.length bss_body > 0 then "\n\n" else "\n")
    ^ "section .text\n"
    ^ String.concat "\n" extern_decls
    ^ (if List.length extern_decls > 0 then "\n\n" else "\n")
    ^ String.concat "\n\n" text_body
;;
