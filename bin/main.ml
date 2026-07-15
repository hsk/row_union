open Row_union
open Eval

let _ =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: row_union <file>";
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let ast = Parser.prog_main Lexer.token lexbuf in
  close_in ch;
  eval_prog ast
