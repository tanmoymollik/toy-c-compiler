open X64_ast

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
  | Data iden, sz ->
    let iden = emit_identifier iden in
    let platform_name = emit_platform_name iden in
    (match Hashtbl.find_opt Core.symbol_map iden with
     | None | Some Core.{ attrs = StaticAttr { init = NoInitial; _ }; _ } ->
       Hashtbl.replace extern_decls platform_name true
     | _ -> ());
    Printf.sprintf "[rel %s %s]" (emit_operand_size sz) platform_name
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

let emit_static_var = function
  | StaticVar { name = Identifier iden; global; init } ->
    let iden = emit_platform_name iden in
    (if global then "global " ^ iden ^ "\n" else "")
    ^ (iden ^ ":\n")
    ^ Printf.sprintf "%s%s dw %s" indent iden (string_of_int init)
  | _ -> assert false
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
  | StaticVar { name = Identifier iden; global; init = 0 } ->
    let iden = emit_platform_name iden in
    let entry =
      (indent ^ "align 4\n")
      ^ (if global then "global " ^ iden ^ "\n" else "")
      ^ Printf.sprintf "%s%s resd 1" indent iden
    in
    Stack.push entry bss_section
  | StaticVar { name = Identifier iden; global; init } ->
    let iden = emit_platform_name iden in
    let entry =
      (indent ^ "align 4\n")
      ^ (if global then "global " ^ iden ^ "\n" else "")
      ^ Printf.sprintf "%s%s dd %s" indent iden (string_of_int init)
    in
    Stack.push entry data_section
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
