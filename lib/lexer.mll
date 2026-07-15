{
open Parser
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter (letter | digit)*
let lident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let uident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "->"                 { ARROW }
  | "|"                  { BAR }
  | ","                  { COMMA }
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | "+"                  { PLUS }
  | "="                  { EQUAL }
  | ":"                  { COLON }
  | "_"                  { WILD }
  | ";;"                 { SEMISEMI }
  | "Int"                { INT }
  | "Bool"               { BOOL }
  | "String"             { STRING }
  | "Bot"                { BOT }
  | "let"                { LET }
  | "rec"                { REC }
  | "in"                 { IN }
  | "fun"                { FUN }
  | "match"              { MATCH }
  | "with"               { WITH }
  | "true"               { BOOL_LIT true }
  | "false"              { BOOL_LIT false }
  | digit+ as n          { INT_LIT (int_of_string n) }
  | "'" (id as v)        { VAR v }
  | lident as id         { ID id }
  | uident as id         { UID id }
  | eof                  { TEOF }
  | _                    { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
