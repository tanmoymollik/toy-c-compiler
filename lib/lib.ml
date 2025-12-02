module Stage = Stage
module Platform = Platform

exception CompileError of string

type compile_args =
  { stage : Stage.stage
  ; platform : Platform.platform
  ; infile : string
  ; outfile : string
  }

module Impl : sig
  val compile : compile_args -> unit
end = struct
  module Validater = Validater.M

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
    | Lexer.SyntaxError e ->
      raise (CompileError ("LexError: " ^ e ^ ": " ^ print_position lexbuf))
    | Parser.Error | Core.ParserError ->
      raise (CompileError ("ParseError: " ^ print_position lexbuf))
  ;;

  let validate stage prog =
    match stage with
    | `Parse ->
      print_endline C_ast.(show_program prog);
      None
    | _ ->
      (try Some (Validater.resolve_program prog) with
       | Validater.SemanticError e -> raise (CompileError ("SemanticError: " ^ e)))
  ;;

  let tacky_gen stage prog =
    match stage with
    | `Validate ->
      print_endline C_ast.(show_program prog);
      None
    | _ -> Some (Tacky_gen.gen_program prog)
  ;;

  let x64_gen stage prog =
    match stage with
    | `Tacky ->
      print_endline Tacky.(show_program prog);
      None
    | _ -> Some (X64_gen.M.gen_program prog)
  ;;

  let codeemit stage platform prog =
    match stage with
    | `CodeGen ->
      print_endline X64_ast.(show_program prog);
      None
    | _ -> Some (Emitter.emit_program platform prog)
  ;;

  let compile = function
    | { stage; platform; infile; outfile } ->
      let inx = In_channel.open_text infile in
      let lexbuf = Lexing.from_channel inx in
      Lexing.set_filename lexbuf infile;
      let ( >>= ) = Option.bind in
      let code =
        parse lexbuf
        >>= validate stage
        >>= tacky_gen stage
        >>= x64_gen stage
        >>= codeemit stage platform
      in
      (match code with
       | Some v ->
         if stage = `CodeEmit then print_endline v;
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
