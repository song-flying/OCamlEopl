{
open Parser
}

let white  = [' ' '\t' '\n']+
let digit  = ['0'-'9']
let int    = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id     = letter (letter|digit)*

rule read = 
  parse
  | white    { read lexbuf }
  | "-"      { DIFF }
  | "+"      { PLUS }
  | ","      { COMMA }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "let"    { LET }
  | "letrec" { LETREC }
  | "="      { EQUALS }
  | "in"     { IN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "zero?"  { ZERO }
  | ">"      { GREATER }
  | "<"      { LESS }
  | "proc"   { PROC }
  | id       { ID (Lexing.lexeme lexbuf) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }
