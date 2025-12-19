%{

open Parser_utils

%}

// Token Definitions
%token <string> IDENT
%token <int32> CONST_INT
%token <Stdint.uint32> CONST_UINT
%token <int64> CONST_LONG
%token <Stdint.uint64> CONST_ULONG
%token <float> CONST_DOUBLE
%token INT LONG DOUBLE VOID
%token SIGNED UNSIGNED STATIC EXTERN
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
%token EQUAL
       PLUS_EQ MINUS_EQ
       ASTERISK_EQ FSLASH_EQ PERCENT_EQ
       LSHIFT_EQ RSHIFT_EQ
       AMPERSAND_EQ PIPE_EQ CARET_EQ
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
  f = list(declaration); EOF { C_ast.Program f }

declaration:
  | v = variable_decl              { C_ast.VarDecl v }
  | f = function_decl              { C_ast.FunDecl f }
  | vf = variable_or_function_decl { vf }

variable_decl:
  specs = nonempty_list(specifier); d = declarator; init = pair(EQUAL, expression); SEMICOLON
    {
      let init = (fun (_, exp) -> exp) init in
      let name, vtp, _ = process_declarator (type_of specs) d in
      (match vtp with
       | Common.FunType _ -> raise (SyntaxError "Variable declarator does not yield variable type")
       | _ -> ());
      C_ast.{ name; init = Some init; vtp; storage = storage specs; }
    }

function_decl:
  | specs = nonempty_list(specifier); d = declarator; body = block
    { function_decl_ast specs d (Some body) }

variable_or_function_decl:
  | specs = nonempty_list(specifier); d = declarator; SEMICOLON
    { parse_declaration specs d }

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
    { C_ast.Break (Common.Identifier "dummy") }
  | CONTINUE; SEMICOLON
    { C_ast.Continue (Common.Identifier "dummy") }
  | WHILE; LPAREN; exp = expression; RPAREN; stmt = statement
    { C_ast.While (exp, stmt, Common.Identifier "dummy") }
  | DO; stmt = statement; WHILE; LPAREN; exp = expression; RPAREN; SEMICOLON
    { C_ast.DoWhile (stmt, exp, Common.Identifier "dummy") }
  | FOR; LPAREN; init = for_init; cnd = option(expression); SEMICOLON; post = option(expression); RPAREN; body = statement
    { C_ast.For { init; cnd; post; body; label = Common.Identifier "dummy" } }
  | SWITCH; LPAREN; cnd = expression; RPAREN; body = statement
    { C_ast.Switch { cnd; body; cases = []; default = false; label = Common.Identifier "dummy" } }
  | CASE; exp = expression; COLON; stmt = statement
    { C_ast.Case (exp, stmt, Common.Identifier "dummy") }
  | DEFAULT; COLON; stmt = statement
    { C_ast.Default (stmt, Common.Identifier "dummy") }
  | SEMICOLON
    { C_ast.Null }

for_init:
  // | d = variable_decl                 { C_ast.InitDecl d }
  | d = declaration
    {
      match d with
       | C_ast.FunDecl _ -> raise (SyntaxError "For loop init cannot be a function declaration")
       | C_ast.VarDecl d -> C_ast.InitDecl d
    }
  | e = option(expression); SEMICOLON { C_ast.InitExp e }

expression:
  | f = factor { f }
  | lexp = expression; bop = bop; rexp = expression
    { C_ast.Binary { bop; lexp; rexp; etp = Common.Int } }
  | lval = expression; aop = aop; rval = expression
    { assignment_ast aop lval rval }
  | cnd = expression; QUESTION; lhs = expression; COLON; rhs = expression
    { C_ast.Conditional { cnd; lhs; rhs; etp = Common.Int } }

factor:
  | c = const                         { C_ast.Constant (c, Common.Int) }
  | id = identifier                   { C_ast.Var (id, Common.Int) }
  | LPAREN;
    specs = nonempty_list(type_specifier);
    d = option(abstract_declarator); RPAREN; exp = factor
    {
      let tp = type_of specs in
      let tgt =
        match d with
        | Some d -> process_abstract_declarator tp d
        | None -> tp
      in
      C_ast.Cast { tgt; exp; etp = Common.Int }
    }
  | uop = uop; exp = factor           { C_ast.Unary (uop, exp, Common.Int) }
  | ASTERISK; exp = factor            { C_ast.Dereference (exp, Common.Int) }
  | AMPERSAND; exp = factor           { C_ast.AddrOf (exp, Common.Int) }
  | tuop = tuop; exp = factor         { C_ast.TUnary (tuop, true, exp, Common.Int) }
  | exp = factor; tuop = tuop         { C_ast.TUnary (tuop, false, exp, Common.Int) }
  | LPAREN; exp = expression; RPAREN  { exp }
  | id = identifier; LPAREN; args = separated_list(COMMA, expression); RPAREN
    { C_ast.FunctionCall (id, args, Common.Int) }

simple_declarator:
  | name = identifier              { Ident name }
  | LPAREN; d = declarator; RPAREN { d }

direct_declarator:
  | sd = simple_declarator; pl = option(param_list)
  {
    match pl with
    | None -> sd
    | Some pl -> FunDeclarator (pl, sd)
  }
  
%inline specifier:
  | ts = type_specifier { ts }
  | EXTERN              { ExternSpec }
  | STATIC              { StaticSpec }

%inline type_specifier:
  | INT       { IntSpec }
  | LONG      { LongSpec }
  | DOUBLE    { DoubleSpec }
  | SIGNED    { SignedSpec }
  | UNSIGNED  { UnsignedSpec }

param_list:
  | LPAREN; VOID; RPAREN
    { [] }
  | LPAREN;
    params =
      separated_nonempty_list(
        COMMA,
        pair(nonempty_list(type_specifier), declarator));
    RPAREN
    { List.map (fun (specs, d) -> Param (type_of specs, d)) params }

declarator:
  | ASTERISK; d = declarator { PointerDeclarator d }
  | dd = direct_declarator { dd }

abstract_declarator:
  | ASTERISK; ad = option(abstract_declarator)
    {
      match ad with
      | Some ad -> AbstractPointer ad
      | None -> AbstractPointer AbstractBase
    }
  | dad = direct_abstract_declarator { dad }

direct_abstract_declarator:
  | LPAREN; ad = abstract_declarator; RPAREN { ad }

%inline const:
  | i = CONST_INT    { Common.ConstInt i }
  | ui = CONST_UINT  { Common.ConstUInt ui }
  | l = CONST_LONG   { Common.ConstLong l }
  | ul = CONST_ULONG { Common.ConstULong ul }
  | d = CONST_DOUBLE { Common.ConstDouble d }

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
  | EQUAL        { Eq } 
  | PLUS_EQ      { AEq }
  | MINUS_EQ     { SEq }
  | ASTERISK_EQ  { MEq }
  | FSLASH_EQ    { DEq }
  | PERCENT_EQ   { REq }
  | LSHIFT_EQ    { LsftEq }
  | RSHIFT_EQ    { RsftEq }
  | AMPERSAND_EQ { BAEq }
  | PIPE_EQ      { BOEq }
  | CARET_EQ     { XEq }

identifier:
  id = IDENT { Common.Identifier id }