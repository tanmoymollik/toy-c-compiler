open Stdint
open Common
open Ast

let indent = String.make 4 ' '

let emit_platform_name = function
  | Identifier name -> name
;;

let data_section : string Stack.t = Stack.create ()
let rodata_section : string Stack.t = Stack.create ()
let bss_section : string Stack.t = Stack.create ()
let text_section : string Stack.t = Stack.create ()

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
  | Bp -> "bp"
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
     | Di | Si | Sp | Bp -> name
     | R8 | R9 | R10 | R11 -> name ^ "w"
     | _ -> assert false)
  | DWord ->
    (match r with
     | Ax | Cx | Dx -> "e" ^ name ^ "x"
     | Di | Si | Sp | Bp -> "e" ^ name
     | R8 | R9 | R10 | R11 -> name ^ "d"
     | _ -> assert false)
  | QWord ->
    (match r with
     | Ax | Cx | Dx -> "r" ^ name ^ "x"
     | Di | Si | Sp | Bp -> "r" ^ name
     | R8 | R9 | R10 | R11 -> name
     | _ -> assert false)
  | AsmDouble ->
    (match r with
     | Ax | Cx | Dx | Di | Si | Sp | Bp | R8 | R9 | R10 | R11 -> emit_reg r QWord
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
  | ByteArray _ -> assert false
;;

let emit_imm ul = function
  | Byte -> Uint8.to_string (Uint64.to_uint8 ul)
  | Word -> Uint16.to_string (Uint64.to_uint16 ul)
  | DWord -> Uint32.to_string (Uint64.to_uint32 ul)
  | _ -> Uint64.to_string ul
;;
