open Brr
open Js_of_ocaml
open Row_union

let stdout_buffer = Buffer.create 1024
let stderr_buffer = Buffer.create 1024

let () =
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer)

let run_code code_str =
  Buffer.clear stdout_buffer;
  Buffer.clear stderr_buffer;
  try
    let lexbuf = Lexing.from_string code_str in
    let ast = Parser.prog_main Lexer.token lexbuf in
    ignore (Eval.eval_prog ast);
    flush stdout; flush stderr;
    (Buffer.contents stdout_buffer, Buffer.contents stderr_buffer)
  with e ->
    flush stdout; flush stderr;
    let err_msg = Printexc.to_string e in
    (Buffer.contents stdout_buffer, Buffer.contents stderr_buffer ^ "\n" ^ err_msg)

let code =
"let rec ev x =
  match x with
  | Add(l, r) -> ev l + ev r
  | i:Int -> i
let rec ev2 x =
  match x with
  | Add(Add(l1, r1), r) -> ev2 l1 + ev2 r1 + ev2 r
  | i:Int -> i
let v = 1
let v = Add(Mul(2,1),3)
let v = ev (Add(Add(1,2),Add(3,4)))
"

let () =
  let doc = G.document in
  let body = Document.body doc in

  let app_container = El.div ~at:[At.class' (Jstr.v "app-container")] [] in
  let left_panel = El.div ~at:[At.class' (Jstr.v "panel left-panel")] [] in
  let right_panel = El.div ~at:[At.class' (Jstr.v "panel right-panel")] [] in

  let textarea = El.textarea ~at:[At.class' (Jstr.v "code-input")] [] in
  let button = El.button ~at:[At.class' (Jstr.v "run-button")] [El.txt (Jstr.v "Run Code")] in
  
  let output_title = El.h3 ~at:[At.class' (Jstr.v "section-title")] [El.txt (Jstr.v "Output")] in
  let output_pre = El.pre ~at:[At.class' (Jstr.v "output-box")] [] in
  
  let error_title = El.h3 ~at:[At.class' (Jstr.v "section-title error-title")] [El.txt (Jstr.v "Error")] in
  let error_pre = El.pre ~at:[At.class' (Jstr.v "error-box")] [] in

  let github_link = El.a 
    ~at:[
      At.class' (Jstr.v "github-link"); 
      At.href (Jstr.v "https://github.com/hsk/row_union");
      At.v (Jstr.v "target") (Jstr.v "_blank")
    ] 
    [El.txt (Jstr.v "GitHub ↗")] 
  in

  let title_container = El.div ~at:[At.class' (Jstr.v "title-container")] [] in
  El.append_children title_container [
    El.h2 ~at:[At.class' (Jstr.v "main-title")] [El.txt (Jstr.v "Row Union")];
    github_link
  ];

  El.set_prop El.Prop.value (Jstr.v code) textarea;

  let on_click _ =
    let input_code = Jstr.to_string (El.prop El.Prop.value textarea) in
    let (out_res, err_res) = run_code input_code in
    El.set_children output_pre [El.txt (Jstr.v out_res)];
    El.set_children error_pre [El.txt (Jstr.v err_res)]
  in
  let _listener = Ev.listen Ev.click on_click (El.as_target button) in

  El.append_children left_panel [
    title_container;
    textarea;
    button
  ];

  El.append_children right_panel [
    output_title; output_pre;
    error_title; error_pre
  ];

  El.append_children app_container [left_panel; right_panel];
  El.append_children body [app_container]
