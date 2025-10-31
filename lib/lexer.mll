{
exception SyntaxError of string
}

let chars = ['a'-'z' 'A'-'Z']
let digits = ['0'-'9']
let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let identifier = (chars | '_')(chars | digits | '_')*
let constant = digits+

rule read =
  parse
  | "int"      { Parser.INT }
  | "void"     { Parser.VOID }
  | "return"   { Parser.RETURN }
  | '('        { Parser.LPAREN }
  | ')'        { Parser.RPAREN }
  | '{'        { Parser.LBRACE }
  | '}'        { Parser.RBRACE }
  | ';'        { Parser.SEMICOLON }
  | "--"       { Parser.DHYPHEN }
  | "<<"       { Parser.LSHIFT }
  | ">>"       { Parser.RSHIFT }
  | "&&"       { Parser.DAMPERSAND }
  | "||"       { Parser.DPIPE }
  | "=="       { Parser.DEQUAL }
  | "!="       { Parser.NEQUAL }
  | "<="       { Parser.LEQUAL }
  | ">="       { Parser.GEQUAL }
  | '+'        { Parser.PLUS }
  | '-'        { Parser.HYPHEN }
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
  | identifier { Parser.IDENT (Lexing.lexeme lexbuf) }
  | constant   { Parser.CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | white      { read lexbuf }
  | newline    { Lexing.new_line lexbuf; read lexbuf }
  | eof        { Parser.EOF }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }