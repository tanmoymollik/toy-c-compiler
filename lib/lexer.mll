{
exception SyntaxError of string

let const_int str =
  try
    if String.ends_with str ~suffix:"l" || String.ends_with str ~suffix:"L"
    then (
      let str = String.sub str 0 (String.length str - 1) in
      Parser.CONST_LONG (Int64.of_string str))
    else (
      let v = Int64.of_string str in
      if v > Int64.of_int32 Int32.max_int then Parser.CONST_LONG v else Parser.CONST_INT (Int64.to_int v))
  with
  | Failure msg ->
    print_endline msg;
    raise (SyntaxError (msg ^ " in integer constant " ^ str))
;;
}

let chars = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']
let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let identifier = (chars | '_')(chars | digits | '_')*
let const_int = digits+
let const_long = const_int ['l' 'L']

rule read =
  parse
  | "int"      { Parser.INT }
  | "long"     { Parser.LONG }
  | "void"     { Parser.VOID }
  | "return"   { Parser.RETURN }
  | "if"       { Parser.IF }
  | "else"     { Parser.ELSE }
  | "goto"     { Parser.GOTO (Lexing.lexeme lexbuf) }
  | "do"       { Parser.DO }
  | "while"    { Parser.WHILE }
  | "for"      { Parser.FOR }
  | "break"    { Parser.BREAK }
  | "continue" { Parser.CONTINUE }
  | "switch"   { Parser.SWITCH }
  | "case"     { Parser.CASE }
  | "default"  { Parser.DEFAULT }
  | "static"   { Parser.STATIC }
  | "extern"   { Parser.EXTERN }
  | '('        { Parser.LPAREN }
  | ')'        { Parser.RPAREN }
  | '{'        { Parser.LBRACE }
  | '}'        { Parser.RBRACE }
  | ';'        { Parser.SEMICOLON }
  | ','        { Parser.COMMA }
  | '?'        { Parser.QUESTION }
  | ':'        { Parser.COLON }
  | "<<="      { Parser.LSHIFT_EQ }
  | ">>="      { Parser.RSHIFT_EQ }
  | "++"       { Parser.DPLUS }
  | "--"       { Parser.DHYPHEN }
  | "<<"       { Parser.LSHIFT }
  | ">>"       { Parser.RSHIFT }
  | "&&"       { Parser.DAMPERSAND }
  | "||"       { Parser.DPIPE }
  | "=="       { Parser.DEQUAL }
  | "!="       { Parser.NEQUAL }
  | "<="       { Parser.LEQUAL }
  | ">="       { Parser.GEQUAL }
  | "+="       { Parser.PLUS_EQ }
  | "-="       { Parser.MINUS_EQ }
  | "*="       { Parser.ASTERISK_EQ }
  | "/="       { Parser.FSLASH_EQ }
  | "%="       { Parser.PERCENT_EQ }
  | "&="       { Parser.AMPERSAND_EQ }
  | "|="       { Parser.PIPE_EQ }
  | "^="       { Parser.CARET_EQ }
  | '+'        { Parser.PLUS }
  | '-'        { Parser.MINUS }
  | '*'        { Parser.ASTERISK }
  | '/'        { Parser.FSLASH }
  | '%'        { Parser.PERCENT }
  | '~'        { Parser.TILDE }
  | '&'        { Parser.AMPERSAND }
  | '|'        { Parser.PIPE }
  | '^'        { Parser.CARET }
  | '!'        { Parser.EXCLAMATION }
  | '<'        { Parser.LESS }
  | '>'        { Parser.GREATER }
  | '='        { Parser.EQUAL }
  | identifier { Parser.IDENT (Lexing.lexeme lexbuf) }
  | const_int  { const_int (Lexing.lexeme lexbuf) }
  | const_long { const_int (Lexing.lexeme lexbuf) }
  | white      { read lexbuf }
  | newline    { Lexing.new_line lexbuf; read lexbuf }
  | eof        { Parser.EOF }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }