%{

open Core.Std
open Solvers.Std

%}

%token <float> NUM
%token <Ast.Var.t> VAR
%token PLUS MINUS TIMES DIV
%token EQUALS SEMI
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start main
%type <Ast.Expr.t> expr
%type <Ast.Stmt.t list> main

%%

main
  : stmts EOF               { $1 }
  ;

stmts
  : rev_stmts               { List.rev $1 }
  ;

rev_stmts
  : stmt                    { [$1] }
  | rev_stmts stmt          { $2 :: $1 }
  ;

stmt
  : expr EQUALS expr eqs SEMI  { Ast.Stmt.Equate ($1, $3, $4) }
  ;

eqs
  : rev_eqs                 { List.rev $1 }
  ;

rev_eqs
  :                         { [] }
  | rev_eqs EQUALS expr     { $3 :: $1 }
  ;

expr
  : NUM                     { Ast.Expr.Const $1 }
  | VAR                     { Ast.Expr.Var $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.Expr.Plus  ($1, $3) }
  | expr MINUS expr         { Ast.Expr.Minus ($1, $3) }
  | expr TIMES expr         { Ast.Expr.Times ($1, $3) }
  | expr DIV expr           { Ast.Expr.Div   ($1, $3) }
  | MINUS expr %prec UMINUS { Ast.Expr.Negate $2 }
;

%%

