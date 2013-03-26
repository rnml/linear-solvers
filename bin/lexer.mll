{
open Core.Std
open Parser
}

rule token = parse
    [' ' '\t' '\n']    { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+ as lxm  { NUM (Float.of_string lxm) }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | eof                { EOF }
