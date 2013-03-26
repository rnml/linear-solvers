%{

open Solvers.Std

let linear_div a b =
  match Linear.value b with
  | None -> failwith "non-constant denominator"
  | Some b -> Linear.div a b

let linear_times a b =
  match Linear.value a with
  | Some a -> Linear.times a b
  | None ->
    match Linear.value b with
    | Some b -> Linear.times b a
    | None -> failwith "neither numerator nor denominator is constant"

%}

%token <float> NUM
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start main
%type <Solvers.Std.Linear.t> main, expr

%%

main
  : expr EOF                { $1 }
  ;

expr
  : NUM                     { Linear.const $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Linear.plus  $1 $3 }
  | expr MINUS expr         { Linear.minus $1 $3 }
  | expr TIMES expr         { linear_times $1 $3 }
  | expr DIV expr           { linear_div   $1 $3 }
  | MINUS expr %prec UMINUS { Linear.negate $2 }
;

%%

