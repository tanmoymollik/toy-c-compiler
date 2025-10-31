// Token Definitions
%token <string> IDENT
%token <int> CONST
%token INT
%token VOID
%token RETURN
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token DHYPHEN
%token PLUS HYPHEN
%token ASTERISK FSLASH PERCENT
%token TILDE 
%token AMPERSAND PIPE CARET 
%token LSHIFT RSHIFT
%token DAMPERSAND DPIPE
%token EXCLAMATION
%token LESS GREATER
%token DEQUAL NEQUAL LEQUAL GEQUAL
%token EOF

// Precedence and associativity
%left DPIPE
%left DAMPERSAND
%left PIPE
%left CARET
%left AMPERSAND
%left DEQUAL NEQUAL
%left LESS GREATER LEQUAL GEQUAL
%left LSHIFT RSHIFT
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
  | HYPHEN       { C_ast.Negate }
  | TILDE        { C_ast.Complement }
  | EXCLAMATION  { C_ast.Not }

%inline bop:
  | PLUS       { C_ast.Add }
  | HYPHEN     { C_ast.Sub }
  | ASTERISK   { C_ast.Mul }
  | FSLASH     { C_ast.Div }
  | PERCENT    { C_ast.Rem }
  | AMPERSAND  { C_ast.BAnd }
  | PIPE       { C_ast.BOr }
  | CARET      { C_ast.Xor }
  | LSHIFT     { C_ast.Lsft }
  | RSHIFT     { C_ast.Rsft }
  | DAMPERSAND { C_ast.And }
  | DPIPE      { C_ast.Or }
  | DEQUAL     { C_ast.Equal }
  | NEQUAL     { C_ast.NEqual }
  | LEQUAL     { C_ast.LEqual }
  | GEQUAL     { C_ast.GEqual }
  | LESS       { C_ast.Less }
  | GREATER    { C_ast.Greater }