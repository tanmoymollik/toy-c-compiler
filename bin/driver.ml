let stage = ref Lib.Stage.Link
let platform = Lib.Platform.Mac
let usage_msg = "Usage: dune exec c_compiler -- [options] <input_file>.c"

let spec =
  [ "--lex", Arg.Unit (fun () -> stage := Lib.Stage.Lex), "Stop after lexing"
  ; "--parse", Arg.Unit (fun () -> stage := Lib.Stage.Parse), "Stop after parsing"
  ; ( "--validate"
    , Arg.Unit (fun () -> stage := Lib.Stage.Validate)
    , "Stop after semantic analysis" )
  ; ( "--tacky"
    , Arg.Unit (fun () -> stage := Lib.Stage.Tacky)
    , "Stop after tacky generation" )
  ; ( "--codegen"
    , Arg.Unit (fun () -> stage := Lib.Stage.CodeGen)
    , "Stop after assembly code generation" )
  ; ( "-S"
    , Arg.Unit (fun () -> stage := Lib.Stage.CodeEmit)
    , "Stop after assembly code emission" )
  ]
;;

exception CommandException of string

let runCommand cmd errMsg = if Sys.command cmd <> 0 then raise (CommandException errMsg)

let postprocess base_name =
  let nasm_cmd =
    match platform with
    | Lib.Platform.Linux -> "nasm -f elf64"
    | Lib.Platform.Mac -> "nasm -f macho64"
  in
  runCommand
    (Printf.sprintf "%s %s.s -o %s.o" nasm_cmd base_name base_name)
    "Error during linking";
  runCommand (Printf.sprintf "rm %s.s" base_name) "Error removing assembly file";
  runCommand
    (Printf.sprintf "gcc -w %s.o -o %s" base_name base_name)
    "Error during linking";
  runCommand (Printf.sprintf "rm %s.o" base_name) "Error removing object file"
;;

let compile args processed_file =
  try
    Lib.compile args;
    runCommand (Printf.sprintf "rm %s" processed_file) "Error removing preprocessed file"
  with
  | CommandException msg ->
    print_endline msg;
    exit 1
  | Lib.CompileException msg ->
    print_endline ("CompileError: " ^ msg);
    (* In case of compile error the preprocessed file was not deleted. Delete it here. *)
    runCommand (Printf.sprintf "rm %s" processed_file) "Error removing preprocessed file";
    exit 1
;;

let drive stage platform infile =
  try
    let base_name = Filename.remove_extension infile in
    runCommand
      (Printf.sprintf "gcc -E -P %s -o %s.i" infile base_name)
      "Error during preprocessing";
    let args : Lib.compile_args =
      { stage
      ; platform
      ; infile = Printf.sprintf "%s.i" base_name
      ; outfile = Printf.sprintf "%s.s" base_name
      }
    in
    compile args args.infile;
    if stage = Lib.Stage.Link then postprocess base_name
  with
  | CommandException msg ->
    print_endline msg;
    exit 1
;;

let input_file = ref ""

let () =
  Arg.parse spec (fun s -> input_file := s) usage_msg;
  if !input_file = ""
  then (
    Arg.usage spec usage_msg;
    exit 1)
  else drive !stage platform !input_file
;;
