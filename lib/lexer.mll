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
  | "(*"                 { comment 1 lexbuf } (* コメント開始（深さ1） *)
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

(* ネスト可能なコメントを処理するルール *)
and comment depth = parse
  | "(*"                 { comment (depth + 1) lexbuf } (* さらにネストした場合は深さを増やす *)
  | "*)"                 { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf } (* 深さが0になれば通常のトークン解析に戻る *)
  | ['\n' '\r' '\n']     { comment depth lexbuf } (* 改行をスキップ *)
  | _                    { comment depth lexbuf } (* コメント内の任意の文字をスキップ *)
  | eof                  { failwith "Unterminated comment at end of file" } (* 閉じられないままEOFに達した場合はエラー *)
