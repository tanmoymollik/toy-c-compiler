open Stdint
open Ast
open Common
open CommonEmitter

let emit_operand = function
  | Imm i, _ -> Printf.sprintf "$%s" (Uint64.to_string i)
  | Reg r, sz -> "%" ^ emit_reg r sz
  | Memory (r, i), _ ->
    let rv = emit_reg r QWord in
    Printf.sprintf "%d(%%%s)" (-i) rv
  | Data iden, _ -> Printf.sprintf "%s(%%rip)" (emit_identifier iden)
;;

let emit_instruction_size = function
  | Byte -> "b"
  | Word -> "w"
  | DWord -> "l"
  | QWord -> "q"
  | AsmDouble -> ""
;;

let emit_instruction = function
  | Mov { src; dst; tp } ->
    let mov =
      match tp with
      | AsmDouble -> "movsd"
      | _ -> "mov" ^ emit_instruction_size tp
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      mov
      (emit_operand (src, tp))
      (emit_operand (dst, tp))
  | Movsx { src; dst } ->
    Printf.sprintf
      "%smovslq %s, %s"
      indent
      (emit_operand (src, DWord))
      (emit_operand (dst, QWord))
  | MovZeroExtend _ ->
    (* Changed to Mov during ins fixing phase. *)
    assert false
  | Lea { src; dst } ->
    Printf.sprintf
      "%sleaq %s, %s"
      indent
      (emit_operand (src, QWord))
      (emit_operand (dst, QWord))
  | Cvttsd2si { src; dst; dst_tp } ->
    Printf.sprintf
      "%scvttsd2si%s %s, %s"
      indent
      (emit_instruction_size dst_tp)
      (emit_operand (src, AsmDouble))
      (emit_operand (dst, dst_tp))
  | Cvtsi2sd { src; dst; src_tp } ->
    Printf.sprintf
      "%scvtsi2sd%s %s, %s"
      indent
      (emit_instruction_size src_tp)
      (emit_operand (src, src_tp))
      (emit_operand (dst, AsmDouble))
  | Unary (uop, src, sz) ->
    Printf.sprintf
      "%s%s%s %s"
      indent
      (emit_uop uop)
      (emit_instruction_size sz)
      (emit_operand (src, sz))
  | Binary { bop; src; dst; tp } ->
    let src_tp =
      match bop with
      | Sal | Sar | Shr -> Byte
      | _ -> tp
    in
    Printf.sprintf
      "%s%s%s %s, %s"
      indent
      (emit_bop (bop, tp))
      (emit_instruction_size tp)
      (emit_operand (src, src_tp))
      (emit_operand (dst, tp))
  | Cmp { lhs; rhs; tp } ->
    let cmp =
      match tp with
      | AsmDouble -> "comisd"
      | _ -> "cmp" ^ emit_instruction_size tp
    in
    Printf.sprintf
      "%s%s %s, %s"
      indent
      cmp
      (emit_operand (rhs, tp))
      (emit_operand (lhs, tp))
  | Idiv (operand, tp) ->
    Printf.sprintf
      "%sidiv%s %s"
      indent
      (emit_instruction_size tp)
      (emit_operand (operand, tp))
  | Div (operand, tp) ->
    Printf.sprintf
      "%sdiv%s %s"
      indent
      (emit_instruction_size tp)
      (emit_operand (operand, tp))
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
      then platform_name ^ "@PLT"
      else platform_name
    in
    Printf.sprintf "%scall %s" indent name
  | Ret -> Printf.sprintf "%sleave\n%sret" indent indent
;;

(* Double is emitted as hex long. *)
let emit_double d = Printf.sprintf "0x%Lx" (Stdlib.Int64.bits_of_float d)

let emit_top_level = function
  | Function { name; global; body } ->
    let name = emit_platform_name name in
    let emit_function_prologue =
      Printf.sprintf "%spush %%rbp\n%smov %%rsp, %%rbp\n" indent indent
    in
    let entry =
      (if global then ".global " ^ name ^ "\n" else "")
      ^ (name ^ ":\n")
      ^ emit_function_prologue
      ^ (List.map emit_instruction body |> String.concat "\n")
    in
    Stack.push entry text_section
  | StaticVar _ -> assert false
  | StaticConstant _ -> assert false
;;

(* | StaticVar { name; global; alignment; init } ->
    let pname = emit_platform_name name in
    let value, is_zero =
      match init with
      | ConstInt i -> Int32.to_string i, i = 0l
      | ConstUInt ui -> Uint32.to_string ui, ui = Uint32.zero
      | ConstLong l -> Int64.to_string l, l = 0L
      | ConstULong ul -> Uint64.to_string ul, ul = Uint64.zero
      | ConstDouble d -> emit_double d, false
    in
    let specifier =
      match AsmSymbolMap.get_var_type name with
      | Byte -> ".byte"
      | Word -> ".short"
      | DWord -> ".long"
      | QWord | AsmDouble -> ".quad"
    in
    let decl =
      if is_zero
      then Printf.sprintf "%s%s %s 1" indent pname specifier
      else Printf.sprintf "%s%s %s %s" indent pname specifier value
    in
    let align = if is_zero then ".alignb" else ".align" in
    let entry =
      Printf.sprintf "%s%s %d\n" indent align alignment
      ^ (if global then indent ^ ".global " ^ pname ^ "\n" else "")
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
         | ConstDouble d -> emit_double d
         | _ -> assert false)
    in
    Stack.push entry rodata_section *)

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
    ".section .text\n"
    ^ String.concat "\n" extern_decls
    ^ (if List.length extern_decls > 0 then "\n\n" else "\n")
    ^ String.concat "\n\n" text_body
    ^ (if List.length text_body > 0 then "\n\n" else "\n")
    ^ ".section .data\n"
    ^ String.concat "\n\n" data_body
    ^ (if List.length data_body > 0 then "\n\n" else "\n")
    ^ ".section .rodata\n"
    ^ String.concat "\n\n" rodata_body
    ^ (if List.length rodata_body > 0 then "\n\n" else "\n")
    ^ ".section .bss\n"
    ^ String.concat "\n\n" bss_body
    ^ (if List.length bss_body > 0 then "\n\n" else "\n")
    ^ ".section .note.GNU-stack,\"\",@progbits\n"
;;
