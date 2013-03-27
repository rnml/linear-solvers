{
open Core.Std
open Parser
}

rule token = parse
    [' ' '\t' '\n']    { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+ as num  { NUM (Float.of_string num) }
  | ['a'-'b']+ as var  { VAR (Float.of_string var) }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '='                { EQUALS }
  | ';'                { SEMI }
  | eof                { EOF }
