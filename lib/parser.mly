%{

type specifier =
  | IntSpec
  | LongSpec
  | StaticSpec
  | ExternSpec

type assign_op =
  | Eq
  | AEq
  | SEq
  | MEq
  | DEq
  | REq
  | BAEq
  | BOEq
  | XEq
  | LsftEq
  | RsftEq

let convert_aop_to_bop = function
  | Eq -> assert false
  | AEq -> C_ast.Add
  | SEq -> C_ast.Sub
  | MEq -> C_ast.Mul
  | DEq -> C_ast.Div
  | REq -> C_ast.Rem
  | BAEq -> C_ast.BAnd
  | BOEq -> C_ast.BOr
  | XEq -> C_ast.Xor
  | LsftEq -> C_ast.Lsft
  | RsftEq -> C_ast.Rsft
;;

let assignment_ast aop lval rval =
  match aop with
  | Eq -> C_ast.Assignment { lval; rval; etp = C_ast.Int }
  | AEq | SEq | MEq | DEq | REq | BAEq | BOEq | XEq | LsftEq | RsftEq ->
    let bop = convert_aop_to_bop aop in
    let bin = C_ast.Binary { bop; lexp = lval; rexp = rval; etp = C_ast.Int } in
    C_ast.Assignment { lval; rval = bin; etp = C_ast.Int }
;;

let process_specs specs =
  let idx_of = function
    | IntSpec -> 0
    | LongSpec -> 1
    | StaticSpec -> 2
    | ExternSpec -> 3
  in
  let cnt = Array.make 4 0 in
  let f spec = cnt.(idx_of spec) <- cnt.(idx_of spec) + 1 in
  List.iter f specs;
  if not (Array.for_all (fun x -> x <= 1) cnt) then raise Core.ParserError;
  if cnt.(0) + cnt.(1) = 0 then raise Core.ParserError;
  if cnt.(2) + cnt.(3) > 1 then raise Core.ParserError;
  cnt
;;

let storage specs =
  let cnt = process_specs specs in
  if cnt.(2) = 1
  then Some C_ast.Static
  else if cnt.(3) = 1
  then Some C_ast.Extern
  else None
;;

let type_of specs =
  let cnt = process_specs specs in
  if cnt.(1) = 1 then C_ast.Long else C_ast.Int
;;

%}

// Token Definitions
%token <string> IDENT
%token <int32> CONST_INT
%token <int64> CONST_LONG
%token INT LONG VOID
%token RETURN
%token IF ELSE
%token <string> GOTO
%token DO WHILE FOR
%token BREAK CONTINUE
%token SWITCH CASE DEFAULT 
%token STATIC EXTERN
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
  | v = variable_decl { C_ast.VarDecl v }
  | f = function_decl { C_ast.FunDecl f }

variable_decl:
  specs = nonempty_list(specifier); name = identifier; init = option(pair(EQUAL, expression)); SEMICOLON
    {
      let init = Option.map (fun (_, exp) -> exp) init in
      C_ast.{ name; init; vtp = type_of specs; storage = storage specs; }
    }
  
%inline specifier:
  | INT    { IntSpec }
  | LONG   { LongSpec }
  | EXTERN { ExternSpec }
  | STATIC { StaticSpec }

%inline type_specifier:
  | INT; LONG         { C_ast.Long }
  | LONG; option(INT) { C_ast.Long }
  | INT               { C_ast.Int }

function_decl:
  | specs = nonempty_list(specifier); name = identifier; LPAREN; params = param_list; RPAREN; body = block
    {
      let ptps = List.map (fun (tp, _) -> tp) params in
      let rtp = type_of specs in
      let params = List.map (fun (_, id) -> id) params in
      C_ast.
        { name
        ; params
        ; body = Some body
        ; ftp = C_ast.FunType { params = ptps; ret = rtp }
        ; storage = storage specs
        }
    }
  | specs = nonempty_list(specifier); name = identifier; LPAREN; params = param_list; RPAREN; SEMICOLON
    {
      let ptps = List.map (fun (tp, _) -> tp) params in
      let rtp = type_of specs in
      let params = List.map (fun (_, id) -> id) params in
      C_ast.
        { name
        ; params
        ; body = None
        ; ftp = C_ast.FunType { params = ptps; ret = rtp }
        ; storage = storage specs
        }
    }

param_list:
  | VOID
    { [] }
  | params = separated_nonempty_list(COMMA, pair(type_specifier, identifier))
    { params }

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
    { C_ast.Binary { bop; lexp; rexp; etp = C_ast.Int } }
  | lval = expression; aop = aop; rval = expression
    { assignment_ast aop lval rval }
  | cnd = expression; QUESTION; lhs = expression; COLON; rhs = expression
    { C_ast.Conditional { cnd; lhs; rhs; etp = C_ast.Int } }

factor:
  | c = const                         { C_ast.Constant (c, C_ast.Int) }
  | id = identifier                   { C_ast.Var (id, C_ast.Int) }
  | LPAREN; tgt = type_specifier; RPAREN; exp = factor
    { C_ast.Cast { tgt; exp; etp = C_ast.Int } }
  | uop = uop; exp = factor           { C_ast.Unary (uop, exp, C_ast.Int) }
  | tuop = tuop; exp = factor         { C_ast.TUnary (tuop, true, exp, C_ast.Int) }
  | exp = factor; tuop = tuop         { C_ast.TUnary (tuop, false, exp, C_ast.Int) }
  | LPAREN; exp = expression; RPAREN  { exp }
  | id = identifier; LPAREN; args = separated_list(COMMA, expression); RPAREN
    { C_ast.FunctionCall (id, args, C_ast.Int) }

%inline const:
  | i = CONST_INT  { C_ast.ConstInt i }
  | l = CONST_LONG { C_ast.ConstLong l }

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
  id = IDENT { C_ast.Identifier id }