open Stdint

exception SyntaxError = Errors.SyntaxError

let raise_error msg str = raise (SyntaxError (msg ^ " in integer constant " ^ str))

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

let escape_char_value = function
  | '\'' -> 39
  | '"' -> 34
  | '?' -> 63
  | '\\' -> 92
  | 'a' -> 7
  | 'b' -> 8
  | 'f' -> 12
  | 'n' -> 10
  | 'r' -> 13
  | 't' -> 9
  | 'v' -> 11
  | _ -> assert false
;;

let parse_char str =
  let c =
    if String.get str 1 = '\\'
    then String.get str 2 |> escape_char_value
    else String.get str 1 |> Char.code
  in
  Parser.CONST_INT (Int32.of_int c)
;;

let parse_string str =
  let str = String.length str - 2 |> String.sub str 1 in
  let _, str =
    String.fold_left
      (fun (b, acc) c ->
         if b
         then (
           let c = escape_char_value c |> Char.chr in
           false, String.make 1 c |> String.cat acc)
         else if c = '\\'
         then true, acc
         else false, String.make 1 c |> String.cat acc)
      (false, "")
      str
  in
  Parser.CONST_STRING str
;;
