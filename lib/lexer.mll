{

open Lexer_utils

}

let chars = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']
let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let identifier = (chars | '_')(chars | digits | '_')*
let long_spec = ['l' 'L']
let unsigned_spec = ['u' 'U']
let const_int = digits+
let const_uint = digits+ unsigned_spec
let const_long = digits+ long_spec
let const_ulong = digits+ (long_spec unsigned_spec | unsigned_spec long_spec)

rule read =
  parse
  | "int"        { Parser.INT }
  | "long"       { Parser.LONG }
  | "signed"     { Parser.SIGNED }
  | "unsigned"   { Parser.UNSIGNED }
  | "static"     { Parser.STATIC }
  | "extern"     { Parser.EXTERN }
  | "void"       { Parser.VOID }
  | "return"     { Parser.RETURN }
  | "if"         { Parser.IF }
  | "else"       { Parser.ELSE }
  | "goto"       { Parser.GOTO (Lexing.lexeme lexbuf) }
  | "do"         { Parser.DO }
  | "while"      { Parser.WHILE }
  | "for"        { Parser.FOR }
  | "break"      { Parser.BREAK }
  | "continue"   { Parser.CONTINUE }
  | "switch"     { Parser.SWITCH }
  | "case"       { Parser.CASE }
  | "default"    { Parser.DEFAULT }
  | '('          { Parser.LPAREN }
  | ')'          { Parser.RPAREN }
  | '{'          { Parser.LBRACE }
  | '}'          { Parser.RBRACE }
  | ';'          { Parser.SEMICOLON }
  | ','          { Parser.COMMA }
  | '?'          { Parser.QUESTION }
  | ':'          { Parser.COLON }
  | "<<="        { Parser.LSHIFT_EQ }
  | ">>="        { Parser.RSHIFT_EQ }
  | "++"         { Parser.DPLUS }
  | "--"         { Parser.DHYPHEN }
  | "<<"         { Parser.LSHIFT }
  | ">>"         { Parser.RSHIFT }
  | "&&"         { Parser.DAMPERSAND }
  | "||"         { Parser.DPIPE }
  | "=="         { Parser.DEQUAL }
  | "!="         { Parser.NEQUAL }
  | "<="         { Parser.LEQUAL }
  | ">="         { Parser.GEQUAL }
  | "+="         { Parser.PLUS_EQ }
  | "-="         { Parser.MINUS_EQ }
  | "*="         { Parser.ASTERISK_EQ }
  | "/="         { Parser.FSLASH_EQ }
  | "%="         { Parser.PERCENT_EQ }
  | "&="         { Parser.AMPERSAND_EQ }
  | "|="         { Parser.PIPE_EQ }
  | "^="         { Parser.CARET_EQ }
  | '+'          { Parser.PLUS }
  | '-'          { Parser.MINUS }
  | '*'          { Parser.ASTERISK }
  | '/'          { Parser.FSLASH }
  | '%'          { Parser.PERCENT }
  | '~'          { Parser.TILDE }
  | '&'          { Parser.AMPERSAND }
  | '|'          { Parser.PIPE }
  | '^'          { Parser.CARET }
  | '!'          { Parser.EXCLAMATION }
  | '<'          { Parser.LESS }
  | '>'          { Parser.GREATER }
  | '='          { Parser.EQUAL }
  | identifier   { Parser.IDENT (Lexing.lexeme lexbuf) }
  | const_int    { parse_int (Lexing.lexeme lexbuf) }
  | const_uint   { parse_uint (Lexing.lexeme lexbuf) }
  | const_long   { parse_long (Lexing.lexeme lexbuf) }
  | const_ulong  { parse_ulong (Lexing.lexeme lexbuf) }
  | white        { read lexbuf }
  | newline      { Lexing.new_line lexbuf; read lexbuf }
  | eof          { Parser.EOF }
  | _            { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }