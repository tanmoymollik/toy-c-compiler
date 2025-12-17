open Stdint
open Ast
open Common

let indent = String.make 4 ' '

let emit_platform_name = function
  | Identifier name -> name
;;

let data_section = Stack.create ()
let rodata_section = Stack.create ()
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
  | Add, AsmDouble -> "addsd"
  | Add, _ -> "add"
  | Sub, AsmDouble -> "subsd"
  | Sub, _ -> "sub"
  | Imul, AsmDouble -> "mulsd"
  | Imul, _ -> "imul"
  | DivDouble, _ -> "divsd"
  | And, _ -> "and"
  | Or, _ -> "or"
  | Xor, AsmDouble -> "xorpd"
  | Xor, _ -> "xor"
  | Sal, _ -> "sal"
  | Sar, _ -> "sar"
  | Shr, _ -> "shr"
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
  | Xmm0 -> "0"
  | Xmm1 -> "1"
  | Xmm2 -> "2"
  | Xmm3 -> "3"
  | Xmm4 -> "4"
  | Xmm5 -> "5"
  | Xmm6 -> "6"
  | Xmm7 -> "7"
  | Xmm8 -> "8"
  | Xmm9 -> "9"
  | Xmm10 -> "10"
  | Xmm11 -> "11"
  | Xmm12 -> "12"
  | Xmm13 -> "13"
  | Xmm14 -> "14"
  | Xmm15 -> "15"
;;

let rec emit_reg r tp =
  let name = emit_reg_name r in
  match tp with
  | Byte ->
    (match r with
     | Ax | Cx | Dx | Di | Si -> name ^ "l"
     | R8 | R9 | R10 | R11 -> name ^ "b"
     | _ -> assert false)
  | Word ->
    (match r with
     | Ax | Cx | Dx -> name ^ "x"
     | Di | Si | Sp -> name
     | R8 | R9 | R10 | R11 -> name ^ "w"
     | _ -> assert false)
  | DWord ->
    (match r with
     | Ax | Cx | Dx -> "e" ^ name ^ "x"
     | Di | Si | Sp -> "e" ^ name
     | R8 | R9 | R10 | R11 -> name ^ "d"
     | _ -> assert false)
  | QWord ->
    (match r with
     | Ax | Cx | Dx -> "r" ^ name ^ "x"
     | Di | Si | Sp -> "r" ^ name
     | R8 | R9 | R10 | R11 -> name
     | _ -> assert false)
  | AsmDouble ->
    (match r with
     | Ax | Cx | Dx | Di | Si | Sp | R8 | R9 | R10 | R11 -> emit_reg r QWord
     | Xmm0
     | Xmm1
     | Xmm2
     | Xmm3
     | Xmm4
     | Xmm5
     | Xmm6
     | Xmm7
     | Xmm8
     | Xmm9
     | Xmm10
     | Xmm11
     | Xmm12
     | Xmm13
     | Xmm14
     | Xmm15 -> "xmm" ^ name)
;;

let emit_operand_size = function
  | Byte -> "byte "
  | Word -> "word "
  | DWord -> "dword "
  | QWord -> "qword "
  | AsmDouble -> ""
;;

let emit_operand = function
  | Imm i, sz -> Printf.sprintf "%s%s" (emit_operand_size sz) (Uint64.to_string i)
  | Reg r, sz -> emit_reg r sz
  | Stack i, sz ->
    if i >= 0
    then Printf.sprintf "%s[rbp - %d]" (emit_operand_size sz) i
    else Printf.sprintf "%s[rbp + %d]" (emit_operand_size sz) (-i)
  | Data iden, sz ->
    let name =
      match sz with
      | AsmDouble -> emit_identifier iden
      | _ -> emit_platform_name iden
    in
    Printf.sprintf "%s[rel %s]" (emit_operand_size sz) name
;;

let emit_instruction = function
  | Mov { src; dst; tp } ->
    let mov =
      match tp with
      | AsmDouble -> "movsd"
      | _ -> "mov"
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      mov
      (emit_operand (dst, tp))
      (emit_operand (src, tp))
  | Movsx { src; dst } ->
    Printf.sprintf
      "%smovsx %s, %s"
      indent
      (emit_operand (dst, QWord))
      (emit_operand (src, DWord))
  | MovZeroExtend _ ->
    (* Changed to Mov during ins fixing phase. *)
    assert false
  | Cvttsd2si { src; dst; dst_tp } ->
    Printf.sprintf
      "%scvttsd2si %s, %s"
      indent
      (emit_operand (dst, dst_tp))
      (emit_operand (src, AsmDouble))
  | Cvtsi2sd { src; dst; src_tp } ->
    Printf.sprintf
      "%scvtsi2sd %s, %s"
      indent
      (emit_operand (dst, AsmDouble))
      (emit_operand (src, src_tp))
  | Unary (uop, src, sz) ->
    Printf.sprintf "%s%s %s" indent (emit_uop uop) (emit_operand (src, sz))
  | Binary { bop; src; dst; tp } ->
    let src_tp =
      match bop with
      | Sal | Sar | Shr -> Byte
      | _ -> tp
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      (emit_bop (bop, tp))
      (emit_operand (dst, tp))
      (emit_operand (src, src_tp))
  | Cmp { lhs; rhs; tp } ->
    let cmp =
      match tp with
      | AsmDouble -> "comisd"
      | _ -> "cmp"
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      cmp
      (emit_operand (lhs, tp))
      (emit_operand (rhs, tp))
  | Idiv (operand, tp) -> Printf.sprintf "%sidiv %s" indent (emit_operand (operand, tp))
  | Div (operand, tp) -> Printf.sprintf "%sdiv %s" indent (emit_operand (operand, tp))
  | Cdq tp ->
    let cmd =
      match tp with
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
      if not (AsmSymbolMap.is_fun_defined name)
      then platform_name ^ " wrt ..plt"
      else platform_name
    in
    Printf.sprintf "%scall %s" indent name
  | Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

let double_to_string d = Printf.sprintf "0x%Lx" (Stdlib.Int64.bits_of_float d)

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
  | StaticVar { name; global; alignment; init } ->
    let pname = emit_platform_name name in
    let value, is_zero =
      match init with
      | ConstInt i -> Int32.to_string i, i = 0l
      | ConstUInt ui -> Uint32.to_string ui, ui = Uint32.zero
      | ConstLong l -> Int64.to_string l, l = 0L
      | ConstULong ul -> Uint64.to_string ul, ul = Uint64.zero
      | ConstDouble d -> double_to_string d, false
    in
    let specifier =
      match AsmSymbolMap.get_var_type name with
      | Byte -> "b"
      | Word -> "w"
      | DWord -> "d"
      | QWord | AsmDouble -> "q"
    in
    let decl =
      if is_zero
      then Printf.sprintf "%s%s res%s 1" indent pname specifier
      else Printf.sprintf "%s%s d%s %s" indent pname specifier value
    in
    let align = if is_zero then "alignb" else "align" in
    let entry =
      Printf.sprintf "%s%s %d\n" indent align alignment
      ^ (if global then indent ^ "global " ^ pname ^ "\n" else "")
      ^ decl
    in
    if is_zero then Stack.push entry bss_section else Stack.push entry data_section
  | StaticConstant { name; alignment; init } ->
    let entry =
      Printf.sprintf
        "%salign %d\n%s%s dq %s"
        indent
        alignment
        indent
        (emit_identifier name)
        (match init with
         | ConstDouble d -> double_to_string d
         | _ -> assert false)
    in
    Stack.push entry rodata_section
;;

let emit_program = function
  | Program tns ->
    List.iter emit_top_level tns;
    let extern_decls = AsmSymbolMap.extern_decls () in
    let extern_decls = List.map (fun s -> "extern " ^ s) extern_decls in
    let f acc e = e :: acc in
    let data_body = Stack.fold f [] data_section in
    let rodata_body = Stack.fold f [] rodata_section in
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
    ^ "section .rodata\n"
    ^ String.concat "\n\n" rodata_body
    ^ (if List.length rodata_body > 0 then "\n\n" else "\n")
    ^ "section .bss\n"
    ^ String.concat "\n\n" bss_body
;;
