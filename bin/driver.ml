type driver_stage =
  [ `LibStage of Lib.Stage.stage
  | `Assemble
  | `Link
  ]

exception CommandError of string

let stage : driver_stage ref = ref `Link
let platform = Lib.Platform.Mac
let usage_msg = "Usage: dune exec c_compiler -- [options] <input_file>.c"

let spec =
  [ "--lex", Arg.Unit (fun () -> stage := `LibStage `Lex), "Stop after lexing"
  ; "--parse", Arg.Unit (fun () -> stage := `LibStage `Parse), "Stop after parsing"
  ; ( "--validate"
    , Arg.Unit (fun () -> stage := `LibStage `Validate)
    , "Stop after semantic analysis" )
  ; ( "--tacky"
    , Arg.Unit (fun () -> stage := `LibStage `Tacky)
    , "Stop after tacky generation" )
  ; ( "--codegen"
    , Arg.Unit (fun () -> stage := `LibStage `CodeGen)
    , "Stop after assembly code generation" )
  ; ( "-S"
    , Arg.Unit (fun () -> stage := `LibStage `CodeEmit)
    , "Stop after assembly code emission" )
  ; "-c", Arg.Unit (fun () -> stage := `Assemble), "Stop after assembly code emission"
  ]
;;

let preprocessed_file base_name = base_name ^ ".i"
let assembly_file base_name = base_name ^ ".s"
let object_file base_name = base_name ^ ".o"
let runCommand cmd errMsg = if Sys.command cmd <> 0 then raise (CommandError errMsg)

let compile args =
  try
    Lib.compile args;
    true
  with
  | Lib.CompileError msg ->
    print_endline ("CompileError: " ^ msg);
    false
;;

let assemble stage base_name =
  match stage with
  | `LibStage _ -> ()
  | `Assemble | `Link ->
    let nasm_cmd =
      match platform with
      | Lib.Platform.Linux -> "nasm -f elf64"
      | Lib.Platform.Mac -> "nasm -f macho64"
    in
    let assembly_file = assembly_file base_name in
    let object_file = object_file base_name in
    runCommand
      (Printf.sprintf "%s %s -o %s" nasm_cmd assembly_file object_file)
      "Error during linking";
    runCommand ("rm " ^ assembly_file) "Error removing assembly file"
;;

let link stage base_name =
  match stage with
  | `LibStage _ | `Assemble -> ()
  | `Link ->
    let object_file = object_file base_name in
    runCommand
      (Printf.sprintf "gcc -w %s -o %s" object_file base_name)
      "Error during linking";
    runCommand ("rm " ^ object_file) "Error removing object file"
;;

let cleanup base_name =
  let preprocessed_file = preprocessed_file base_name in
  let assembly_file = assembly_file base_name in
  let object_file = object_file base_name in
  runCommand
    (Printf.sprintf "rm -f %s %s %s" preprocessed_file assembly_file object_file)
    "Error removing auxiliary files"
;;

let drive_single stage platform infile =
  try
    let base_name = Filename.remove_extension infile in
    let preprocessed_file = preprocessed_file base_name in
    let assembly_file = assembly_file base_name in
    let lib_stage =
      match stage with
      | `LibStage ls -> ls
      | _ -> `CodeEmit
    in
    let args : Lib.compile_args =
      { stage = lib_stage; platform; infile = preprocessed_file; outfile = assembly_file }
    in
    runCommand
      (Printf.sprintf "gcc -E -P %s -o %s" infile preprocessed_file)
      "Error during preprocessing";
    let compile_success = compile args in
    runCommand ("rm " ^ preprocessed_file) "Error removing preprocessed file";
    if not compile_success then exit 1;
    assemble stage base_name;
    link stage base_name
  with
  | CommandError msg ->
    print_endline ("CommandError: " ^ msg);
    cleanup (Filename.remove_extension infile);
    exit 1
;;

let input_file = ref ""

let () =
  Arg.parse spec (fun s -> input_file := s) usage_msg;
  if !input_file = ""
  then (
    Arg.usage spec usage_msg;
    exit 1)
  else drive_single !stage platform !input_file
;;
