// Token Definitions
%token <string> IDENT
%token <int> CONST
%token INT VOID
%token RETURN
%token IF ELSE
%token <string> GOTO
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON QUESTION COLON
%token DPLUS DHYPHEN
%token PLUS MINUS
%token ASTERISK FSLASH PERCENT
%token TILDE 
%token AMPERSAND PIPE CARET 
%token LSHIFT RSHIFT
%token DAMPERSAND DPIPE
%token EXCLAMATION
%token LESS GREATER
%token DEQUAL NEQUAL LEQUAL GEQUAL
%token EQUAL PLUS_EQ MINUS_EQ ASTERISK_EQ FSLASH_EQ PERCENT_EQ LSHIFT_EQ RSHIFT_EQ AMPERSAND_EQ PIPE_EQ CARET_EQ
%token EOF

// Precedence and associativity
%right EQUAL
       PLUS_EQ MINUS_EQ
       ASTERISK_EQ FSLASH_EQ PERCENT_EQ
       LSHIFT_EQ RSHIFT_EQ
       AMPERSAND_EQ PIPE_EQ CARET_EQ
%right QUESTION COLON
%left DPIPE
%left DAMPERSAND
%left PIPE
%left CARET
%left AMPERSAND
%left DEQUAL NEQUAL
%left LESS GREATER LEQUAL GEQUAL
%left LSHIFT RSHIFT
%left PLUS MINUS
%left ASTERISK FSLASH PERCENT
%left DPLUS DHYPHEN

%start <C_ast.program> prog
%%

prog:
  f = function_def; EOF { C_ast.Program f }

function_def:
  INT; name = identifier; LPAREN; VOID; RPAREN; body = block
    { C_ast.Function { name; body; } }

block:
  LBRACE; items = list(block_item); RBRACE { C_ast.Block items }

block_item:
  | s = statement   { C_ast.S s }
  | d = declaration { C_ast.D d }

statement:
  | RETURN; exp = expression; SEMICOLON
    { C_ast.Return exp }
  | exp = expression; SEMICOLON                              
    { C_ast.Expression exp }
  | IF; LPAREN; cnd = expression; RPAREN; thn = statement; els = option(pair(ELSE, statement))
    {
      let els = Option.map (fun (_, stmt) -> stmt) els in
      C_ast.If { cnd; thn; els }
    }
  | GOTO; label = identifier; SEMICOLON
    { C_ast.Goto label }
  | label = identifier; COLON; stmt = statement
    { C_ast.Label (label, stmt) }
  | block = block
    { C_ast.Compound block }
  | SEMICOLON
    { C_ast.Null }

declaration:
  INT; name = identifier; init = option(pair(EQUAL, expression)); SEMICOLON
  {
    let init = Option.map (fun (_, exp) -> exp) init in
    C_ast.Declaration { name; init }
  }

expression:
  | f = factor { f }
  | lexp = expression; bop = bop; rexp = expression
    { C_ast.Binary { bop; lexp; rexp } }
  | lval = expression; aop = aop; rval = expression
    { C_ast.Assignment { aop; lval; rval } }
  | cnd = expression; QUESTION; lhs = expression; COLON; rhs = expression
    { C_ast.Conditional { cnd; lhs; rhs } }

factor:
  | i = CONST                        { C_ast.Constant i }
  | id = identifier                  { C_ast.Var id }
  | uop = uop; exp = factor          { C_ast.Unary (uop, exp) }
  | tuop = tuop; exp = factor        { C_ast.TUnary (tuop, true, exp) }
  | exp = factor; tuop = tuop        { C_ast.TUnary (tuop, false, exp) }
  | LPAREN; exp = expression; RPAREN { exp }
    
%inline uop:
  | MINUS        { C_ast.Negate }
  | TILDE        { C_ast.Complement }
  | EXCLAMATION  { C_ast.Not }

%inline tuop:
  | DPLUS        { C_ast.Inc }
  | DHYPHEN      { C_ast.Dec }

%inline bop:
  | PLUS       { C_ast.Add }
  | MINUS      { C_ast.Sub }
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

%inline aop:
  | EQUAL        { C_ast.Eq } 
  | PLUS_EQ      { C_ast.AEq }
  | MINUS_EQ     { C_ast.SEq }
  | ASTERISK_EQ  { C_ast.MEq }
  | FSLASH_EQ    { C_ast.DEq }
  | PERCENT_EQ   { C_ast.REq }
  | LSHIFT_EQ    { C_ast.LsftEq }
  | RSHIFT_EQ    { C_ast.RsftEq }
  | AMPERSAND_EQ { C_ast.BAEq }
  | PIPE_EQ      { C_ast.BOEq }
  | CARET_EQ     { C_ast.XEq }

identifier:
  id = IDENT { C_ast.Identifier id }