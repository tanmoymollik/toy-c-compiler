module Stage = Stage
module Arch = Arch
module Optimizations = Optimizer.Optimizations

exception CompileError of string

type compile_args =
  { stage : Stage.stage
  ; target : Arch.target
  ; optimizations : Optimizations.level list
  ; infile : string
  ; outfile : string
  ; dump : bool
  ; gas_emit : bool
  }

module Impl : sig
  val compile : compile_args -> unit
end = struct
  module Validater = Validater.M

  let astdump = ref false

  let print_position lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.sprintf
      "%s:%d:%d: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
      (Lexing.lexeme lexbuf)
  ;;

  let parse lexbuf =
    try Some (Parser.prog Lexer.read lexbuf) with
    | Errors.SyntaxError e ->
      raise (CompileError ("LexError: " ^ e ^ ": " ^ print_position lexbuf))
    | Parser.Error -> raise (CompileError ("ParseError: " ^ print_position lexbuf))
  ;;

  let validate stage prog =
    match stage with
    | `Parse ->
      if !astdump then print_endline C_ast.(show_program prog);
      None
    | _ ->
      (try Some (Validater.resolve_program prog) with
       | Errors.SemanticError e -> raise (CompileError ("SemanticError: " ^ e)))
  ;;

  let tacky_gen stage prog =
    match stage with
    | `Validate ->
      if !astdump then print_endline C_ast.(show_program prog);
      None
    | _ -> Some (Tacky.Gen.gen_program prog)
  ;;

  type asm_prog =
    | X64 of X64.Ast.program
    | Riscv64 of Riscv64.Ast.program

  let code_gen stage target prog =
    match stage with
    | `Tacky ->
      if !astdump then print_endline Tacky.Ast.(show_program prog);
      None
    | _ ->
      AsmSymbolMap.gen_asm_symbol_map ();
      (match target with
       | Arch.X86_64 -> Some (X64 (X64.Gen.gen_program prog))
       | Arch.RISCV64 -> Some (Riscv64 (Riscv64.Gen.gen_program prog)))
  ;;

  let code_emit stage gas_emit prog =
    match stage with
    | `CodeGen ->
      if !astdump
      then (
        match prog with
        | X64 prog -> print_endline X64.Ast.(show_program prog)
        | Riscv64 prog -> print_endline Riscv64.Ast.(show_program prog));
      None
    | _ ->
      (match prog with
       | X64 prog ->
         let prog =
           if gas_emit
           then X64.GasEmitter.emit_program prog
           else X64.NasmEmitter.emit_program prog
         in
         Some prog
       | Riscv64 prog ->
         assert gas_emit;
         Some (Riscv64.GasEmitter.emit_program prog))
  ;;

  let compile = function
    | { stage; target; infile; outfile; dump; gas_emit; _ } ->
      astdump := dump;
      let inx = In_channel.open_text infile in
      let lexbuf = Lexing.from_channel inx in
      Lexing.set_filename lexbuf infile;
      let ( >>= ) = Option.bind in
      let code =
        parse lexbuf
        >>= validate stage
        >>= tacky_gen stage
        >>= code_gen stage target
        >>= code_emit stage gas_emit
      in
      (match code with
       | Some v ->
         let oc = open_out outfile in
         output_string oc v;
         close_out oc
       | None -> ())
  ;;
end

(** Compiles the given file.
    @raise CompileError if an error is encountered.
*)
let compile args = Impl.compile args
