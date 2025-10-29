// Token Definitions
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
%token PLUS
%token HYPHEN
%token ASTERISK
%token FSLASH
%token PERCENT
%token TILDE 
%token SEMICOLON
%token EOF

// Precedence and associativity
%left PLUS HYPHEN
%left ASTERISK FSLASH PERCENT

%start <C_ast.program> prog
%%

prog:
  f = function_def; EOF { C_ast.Program f }

function_def:
  INT; name = identifier; LPAREN; VOID; RPAREN; LBRACE; body = statement; RBRACE
    { C_ast.Function { name; body; } }

identifier:
  id = IDENT { C_ast.Identifier id }

statement:
  RETURN; exp = expression; SEMICOLON { C_ast.Return exp }

expression:
  | f = factor { f }
  | lexp = expression; bop = bop; rexp = expression
    { C_ast.Binary { bop; lexp; rexp } }

factor:
  | i = CONST                        { C_ast.Constant i }
  | uop = uop; exp = factor          { C_ast.Unary (uop, exp) }
  | LPAREN; exp = expression; RPAREN { exp }
    
%inline uop:
  | HYPHEN { C_ast.Negate }
  | TILDE  { C_ast.Complement }

%inline bop:
  | PLUS     { C_ast.Add }
  | HYPHEN   { C_ast.Sub }
  | ASTERISK { C_ast.Mul }
  | FSLASH   { C_ast.Div }
  | PERCENT  { C_ast.Rem }