%{
open Ast
let rec make_lam vars body =
  match vars with
  | [] -> body
  | v :: vs -> Lam(v, make_lam vs body)
%}
%token INT BOOL STRING BOT ARROW BAR COMMA LPAREN RPAREN TEOF
%token <string> ID UID VAR
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token LET REC EQUAL IN FUN PLUS MATCH WITH COLON WILD SEMISEMI
%right ARROW
%start t_main expr_main prog_main
%type <Ast.t> t_main
%type <Ast.expr> expr_main
%type <Ast.decl list> prog_main
%%
t_main:
    t TEOF                          { $1 }
expr_main:
    expr TEOF                       { $1 }
prog_main:
    opt_semisemis decl_list TEOF    { $2 }
  | opt_semisemis TEOF              { [] }
opt_semisemis:
    SEMISEMI opt_semisemis          { () }
  | /* empty */                     { () }
decl_list:
    decl opt_semisemis decl_list    { $1 :: $3 }
  | decl opt_semisemis              { [$1] }
decl:
    LET ID EQUAL expr               { LetDecl($2, $4) }
  | LET REC ID id_list EQUAL expr   { LetDecl($3, Fix($3, make_lam $4 $6)) }
  | expr                            { ExprDecl($1) }
t:
    arrow_t { $1 }
arrow_t:
    union_t ARROW arrow_t           { [Arrow($1, $3)] }
  | union_t                         { $1 }
union_t:
    atom_t BAR union_t              { $1 @ $3 }
  | atom_t                          { $1 }
atom_t:
    INT                             { [Int] }
  | BOOL                            { [Bool] }
  | STRING                          { [String] }
  | BOT                             { [] }
  | VAR                             { [Var (get_tvar $1)] }
  | UID LPAREN t_list RPAREN        { [Const($1, $3)] }
  | UID                             { [Const($1, [])] }
  | LPAREN t RPAREN                 { $2 }
t_list:
    t COMMA t_list                  { $1 :: $3 }
  | t                               { [$1] }

expr:
    LET ID EQUAL expr IN expr       { Let($2, $4, $6) }
  | LET REC ID id_list EQUAL expr IN expr
                                    { Let($3, Fix($3, make_lam $4 $6), $8) }
  | FUN ID ARROW expr               { Lam($2, $4) }
  | MATCH expr WITH opt_bar branches{ Match($2, $5) }
  | expr_add                        { $1 }
id_list:
    ID id_list                      { $1 :: $2 }
  | ID                              { [$1] }
expr_add:
    expr_add PLUS expr_app          { Add($1, $3) }
  | expr_app                        { $1 }
expr_app:
    expr_app expr_atom              { App($1, $2) }
  | expr_atom                       { $1 }
expr_atom:
    ID                              { VarExpr($1) }
  | INT_LIT                         { IntExpr($1) }
  | BOOL_LIT                        { BoolExpr($1) }
  | UID LPAREN expr_list RPAREN     { ConstApp($1, $3) }
  | LPAREN expr RPAREN              { $2 }
expr_list:
    expr COMMA expr_list            { $1 :: $3 }
  | expr                            { [$1] }
opt_bar:
    BAR                             { () }
  | /* empty */                     { () }
branches:
    branch BAR branches             { $1 :: $3 }
  | branch                          { [$1] }
branch:
    pattern ARROW expr              { ($1, $3) }
pattern:
    INT_LIT                         { PInt($1) }
  | BOOL_LIT                        { PBool($1) }
  | WILD                            { PDefault }
  | ID COLON union_t                { PAnn($1, $3) }
  | UID LPAREN pattern_list RPAREN  { PConst($1, $3) }
  | ID                              { PVar($1) }
pattern_list:
    pattern COMMA pattern_list      { $1 :: $3 }
  | pattern                         { [$1] }
