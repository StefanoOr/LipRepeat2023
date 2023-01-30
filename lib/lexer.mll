{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let number = ['0'-'9']
let const = number+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "repeat" { REPEAT }
  | "forever" { FOREVER }
  | "not " { NOT }
  | "and" { AND }
  | "or" { OR }
  | ":=" { ASSIGN }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | ";" { SEQ }
  | "int" { INTDEC }
  | "skip" { SKIP }
  | "proc" { PROC }
  | "array" { ARRAY }
  | "break" { BREAK }
  | "ref" { REF }
  | "val" { VAL }
  | id { ID (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
