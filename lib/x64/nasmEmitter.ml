open Stdint
open Ast
open Common
open CommonEmitter

let emit_operand_size = function
  | Byte -> "byte "
  | Word -> "word "
  | DWord -> "dword "
  | QWord -> "qword "
  | AsmDouble -> ""
  | ByteArray _ -> assert false
;;

let emit_operand = function
  | Imm i, sz -> Printf.sprintf "%s%s" (emit_operand_size sz) (Uint64.to_string i)
  | Reg r, sz -> emit_reg r sz
  | Pseudo _, _ -> assert false
  | Memory (r, i), sz ->
    let rv = emit_reg r QWord in
    if i >= 0
    then Printf.sprintf "%s[%s - %d]" (emit_operand_size sz) rv i
    else Printf.sprintf "%s[%s + %d]" (emit_operand_size sz) rv (-i)
  | Data iden, sz ->
    let name =
      match sz with
      | AsmDouble -> emit_identifier iden
      | _ -> emit_platform_name iden
    in
    Printf.sprintf "%s[rel %s]" (emit_operand_size sz) name
  | Indexed { base; ind; scale }, sz ->
    Printf.sprintf
      "%s[%s + %s * %d]"
      (emit_operand_size sz)
      (emit_reg base QWord)
      (emit_reg ind QWord)
      scale
  | PseudoMem _, _ -> assert false
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
  | Lea { src; dst } ->
    Printf.sprintf
      "%slea %s, %s"
      indent
      (emit_operand (dst, QWord))
      (emit_operand (src, QWord))
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
  | JmpP iden -> Printf.sprintf "%sjp .L%s" indent (emit_identifier iden)
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

let emit_double d = Printf.sprintf "0x%Lx" (Stdlib.Int64.bits_of_float d)

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
  | StaticVar { name; global; alignment; init_list } ->
    let pname = emit_platform_name name in
    let emit_static_init = function
      | IntInit i -> "dd " ^ Int32.to_string i
      | UIntInit ui -> "dd " ^ Uint32.to_string ui
      | LongInit l -> "dq " ^ Int64.to_string l
      | ULongInit ul -> "dq " ^ Uint64.to_string ul
      | DoubleInit d -> "dq " ^ emit_double d
      | ZeroInit { bytes } -> Printf.sprintf "times %d db 0" bytes
    in
    let static_inits = List.map emit_static_init init_list in
    let static_inits = String.concat (",\n" ^ indent) static_inits in
    let entry =
      Printf.sprintf "%salign %d\n" indent alignment
      ^ (if global then indent ^ "global " ^ pname ^ "\n" else "")
      ^ (pname ^ ":\n")
      ^ Printf.sprintf "%s%s" indent static_inits
    in
    Stack.push entry data_section
  | StaticConstant { name; alignment; init } ->
    let entry =
      Printf.sprintf
        "%salign %d\n%s%s dq %s"
        indent
        alignment
        indent
        (emit_identifier name)
        (match init with
         | DoubleInit d -> emit_double d
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
