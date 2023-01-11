{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr+
let int = '-'?digit+

rule read =
  parse
  | whire { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LSQPAREN }
  | "]" { RSQPAREN }
  | "{" { LBRACKET }
  | "}" { RBRACKET }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "=" { EQ }
  | "<=" { LEQ }
  | ";" { SEQ }
  | "skip" { SKIP }
  | "break" { BREAK }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "repeat" { REPEAT }
  | "forever" { FOREVER }
  | "int" { INT }
  | "array" { ARRAY }
  | "proc" { PROC }
  | "ref" { REF }
  | "val" { VAL }
  | id { ID (Lexing.lexeme lexbuf) }
  | digit { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }