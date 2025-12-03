open Stdint

exception SyntaxError = Errors.SyntaxError

let raise_error msg str =
  print_endline msg;
  raise (SyntaxError (msg ^ " in integer constant " ^ str))
;;

let parse_int str =
  try
    let v = Int64.of_string str in
    if v > Int64.of_int32 Int32.max_int
    then Parser.CONST_LONG v
    else Parser.CONST_INT (Int64.to_int32 v)
  with
  | Failure msg -> raise_error msg str
;;

let parse_uint str =
  assert (String.ends_with str ~suffix:"u" || String.ends_with str ~suffix:"U");
  try
    let str = String.sub str 0 (String.length str - 1) in
    let v = Uint64.of_string str in
    if v > Uint64.of_uint32 Uint32.max_int
    then Parser.CONST_ULONG v
    else Parser.CONST_UINT (Uint64.to_uint32 v)
  with
  | Failure msg -> raise_error msg str
;;

let parse_long str =
  assert (String.ends_with str ~suffix:"l" || String.ends_with str ~suffix:"L");
  try
    let str = String.sub str 0 (String.length str - 1) in
    Parser.CONST_LONG (Int64.of_string str)
  with
  | Failure msg -> raise_error msg str
;;

let parse_ulong str =
  assert (
    String.ends_with str ~suffix:"ul"
    || String.ends_with str ~suffix:"uL"
    || String.ends_with str ~suffix:"Ul"
    || String.ends_with str ~suffix:"UL");
  try
    let str = String.sub str 0 (String.length str - 2) in
    Parser.CONST_ULONG (Uint64.of_string str)
  with
  | Failure msg -> raise_error msg str
;;
