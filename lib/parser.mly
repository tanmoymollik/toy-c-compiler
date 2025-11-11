// Token Definitions
%token <string> IDENT
%token <int> CONST
%token INT VOID
%token RETURN
%token IF ELSE
%token <string> GOTO
%token DO WHILE FOR
%token BREAK CONTINUE
%token SWITCH CASE DEFAULT 
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON COMMA
%token QUESTION COLON
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
  f = list(function_decl); EOF { C_ast.Program f }

declaration:
  | v = variable_decl { C_ast.VarDecl v }
  | f = function_decl { C_ast.FunDecl f }

variable_decl:
  INT; name = identifier; init = option(pair(EQUAL, expression)); SEMICOLON
    {
      let init = Option.map (fun (_, exp) -> exp) init in C_ast.{ name; init }
    }

function_decl:
  | INT; name = identifier; LPAREN; params = param_list; RPAREN; body = block
    { C_ast.{ name; params; body = Some body; } }
  | INT; name = identifier; LPAREN; params = param_list; RPAREN; SEMICOLON
    { C_ast.{ name; params; body = None; } }

param_list:
  | VOID
    { [] }
  | params = separated_nonempty_list(COMMA, pair(INT, identifier))
    { List.map (fun (_, id) -> id) params }

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
  | BREAK; SEMICOLON
    { C_ast.Break (C_ast.Identifier "dummy") }
  | CONTINUE; SEMICOLON
    { C_ast.Continue (C_ast.Identifier "dummy") }
  | WHILE; LPAREN; exp = expression; RPAREN; stmt = statement
    { C_ast.While (exp, stmt, C_ast.Identifier "dummy") }
  | DO; stmt = statement; WHILE; LPAREN; exp = expression; RPAREN; SEMICOLON
    { C_ast.DoWhile (stmt, exp, C_ast.Identifier "dummy") }
  | FOR; LPAREN; init = for_init; cnd = option(expression); SEMICOLON; post = option(expression); RPAREN; body = statement
    { C_ast.For { init; cnd; post; body; label = C_ast.Identifier "dummy" } }
  | SWITCH; LPAREN; cnd = expression; RPAREN; body = statement
    { C_ast.Switch { cnd; body; cases = []; default = false; label = C_ast.Identifier "dummy" } }
  | CASE; exp = expression; COLON; stmt = statement
    { C_ast.Case (exp, stmt, C_ast.Identifier "dummy") }
  | DEFAULT; COLON; stmt = statement
    { C_ast.Default (stmt, C_ast.Identifier "dummy") }
  | SEMICOLON
    { C_ast.Null }

for_init:
  | d = variable_decl                 { C_ast.InitDecl d }
  | e = option(expression); SEMICOLON { C_ast.InitExp e }

expression:
  | f = factor { f }
  | lexp = expression; bop = bop; rexp = expression
    { C_ast.Binary { bop; lexp; rexp } }
  | lval = expression; aop = aop; rval = expression
    { C_ast.Assignment { aop; lval; rval } }
  | cnd = expression; QUESTION; lhs = expression; COLON; rhs = expression
    { C_ast.Conditional { cnd; lhs; rhs } }

factor:
  | i = CONST                         { C_ast.Constant i }
  | id = identifier                   { C_ast.Var id }
  | uop = uop; exp = factor           { C_ast.Unary (uop, exp) }
  | tuop = tuop; exp = factor         { C_ast.TUnary (tuop, true, exp) }
  | exp = factor; tuop = tuop         { C_ast.TUnary (tuop, false, exp) }
  | LPAREN; exp = expression; RPAREN  { exp }
  | id = identifier; LPAREN; args = separated_list(COMMA, expression); RPAREN
    { C_ast.FunctionCall (id, args) }

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