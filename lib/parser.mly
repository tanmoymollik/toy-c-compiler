%token <string> IDENT
%token <int> CONST
%token INT
%token VOID
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token DHYPHEN
%token HYPHEN
%token TILDE 
%token SEMICOLON
%token EOF
%start <C_ast.program> prog
%%

prog:
  f = function_def; EOF { C_ast.Program f }

function_def:
  INT; name = identifier; LPAREN; VOID; RPAREN; LBRACE; body = statement; RBRACE { C_ast.Function {name; body;} }

identifier:
  id = IDENT { C_ast.Identifier id }

statement:
  RETURN; exp = expression; SEMICOLON { C_ast.Return exp }

expression:
  | i = CONST { C_ast.Constant i }
  | uop = unop; exp = expression { C_ast.Unary (uop, exp) }
  | LPAREN; exp = expression; RPAREN { exp }
    
unop:
  | HYPHEN { C_ast.Negate }
  | TILDE  { C_ast.Complement }