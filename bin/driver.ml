type driver_stage =
  [ `LibStage of Lib.Stage.stage
  | `Assemble
  | `Link
  ]

exception CommandError of string

let stage : driver_stage ref = ref `Link
let target = ref Lib.Arch.X86_64
let astdump = ref false
let link_math = ref false
let optimizations = ref []
let use_gas = ref false
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
  ; "-c", Arg.Unit (fun () -> stage := `Assemble), "Generate object file only"
  ; ( "--riscv64"
    , Arg.Unit
        (fun () ->
          target := Lib.Arch.RISCV64;
          (* Gas format is always used for riscv64 *)
          use_gas := true)
    , "Generate code for riscv64 target" )
  ; "-lm", Arg.Unit (fun () -> link_math := true), "Link the math library"
  ; ( "--fold-constants"
    , Arg.Unit
        (fun () -> optimizations := Lib.Optimizations.FoldConstants :: !optimizations)
    , "Optimization - fold constants" )
  ; ( "--propagate-copies"
    , Arg.Unit
        (fun () -> optimizations := Lib.Optimizations.PropagateCopies :: !optimizations)
    , "Optimization - propagate copies" )
  ; ( "--eliminate-unreachable-code"
    , Arg.Unit
        (fun () ->
          optimizations := Lib.Optimizations.EliminateUnreachableCode :: !optimizations)
    , "Optimization - eliminate unreachable code" )
  ; ( "--eliminate-dead-stores"
    , Arg.Unit
        (fun () ->
          optimizations := Lib.Optimizations.EliminateDeadStores :: !optimizations)
    , "Optimization - eliminate dead stores" )
  ; "--gas", Arg.Unit (fun () -> use_gas := true), "Emit code in GAS syntax"
  ; "--dump", Arg.Unit (fun () -> astdump := true), "Dump ast"
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
    let as_cmd =
      match !target with
      | Lib.Arch.X86_64 -> if !use_gas then "as" else "nasm -f elf64"
      | Lib.Arch.RISCV64 -> "riscv64-linux-gnu-as"
    in
    let assembly_file = assembly_file base_name in
    let object_file = object_file base_name in
    runCommand
      (Printf.sprintf "%s %s -o %s" as_cmd assembly_file object_file)
      "Error during linking";
    runCommand ("rm " ^ assembly_file) "Error removing assembly file"
;;

let link stage base_name =
  match stage with
  | `LibStage _ | `Assemble -> ()
  | `Link ->
    let object_file = object_file base_name in
    let gcc_cmd =
      match !target with
      | Lib.Arch.X86_64 -> "gcc"
      | Lib.Arch.RISCV64 -> "riscv64-linux-gnu-gcc"
    in
    let link_math = if !link_math then "-lm" else "" in
    runCommand
      (Printf.sprintf "%s -w %s -o %s %s" gcc_cmd object_file base_name link_math)
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

let drive_single stage target infile =
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
      { stage = lib_stage
      ; target
      ; optimizations = !optimizations
      ; infile = preprocessed_file
      ; outfile = assembly_file
      ; dump = !astdump
      ; gas_emit = !use_gas
      }
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
  else drive_single !stage !target !input_file
;;
