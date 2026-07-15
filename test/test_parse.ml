open Row_union
open Ast
open Typing
open Eval
open Test_typing

let assert_type_compatible inferred expected test_name =
  if alpha_equal inferred expected then
    Printf.printf "[OK] %s\n" test_name
  else
    failwith (Printf.sprintf "[FAIL] %s\n  Inferred: %s\n  Expected: %s" 
                test_name (show_t inferred) (show_t expected))

let test_parse_t test_name s =
  try
    Parser.t_main Lexer.token (Lexing.from_string s)
  with Parsing.Parse_error ->
    failwith (Printf.sprintf "【型パース失敗】テスト [%s] でエラー。対象文字列: \"%s\"" test_name s)

let test_parse_expr test_name s =
  try
    Parser.expr_main Lexer.token (Lexing.from_string s)
  with Parsing.Parse_error ->
    failwith (Printf.sprintf "【式パース失敗】テスト [%s] でエラー。対象文字列: \"%s\"" test_name s)

let test_parse_prog test_name s =
  try
    Parser.prog_main Lexer.token (Lexing.from_string s)
  with Parsing.Parse_error ->
    failwith (Printf.sprintf "【式パース失敗】テスト [%s] でエラー。対象文字列: \"%s\"" test_name s)

let _ =
  print_endline "Running type system tests..."

let _ =
  reset_tvars ();
  assert_true (subtype [] (test_parse_t "sub_union" "Int | Bool") (test_parse_t "sub_union" "Bool | Int")) "sub_union";
  assert_true (subtype [] (test_parse_t "sub_union2" "Int | Bool") (test_parse_t "sub_union2" "Int | Bool | String")) "sub_union2";
  assert_true (not (subtype [] (test_parse_t "sub_union_fail" "Int | Bool | String") (test_parse_t "sub_union_fail" "Int | Bool"))) "sub_union_fail";
  assert_true (subtype [] (test_parse_t "sub_arrow" "(Int | Bool) -> Int") (test_parse_t "sub_arrow" "Int -> (Int | Bool)")) "sub_arrow"

let _ =
  reset_tvars ();
  let e_lam = test_parse_expr "lam_apply" "(fun x -> x + 1) 1" in
  assert_type_compatible (check [] e_lam) (test_parse_t "lam_apply" "Int") "lam_apply"

let _ =
  reset_tvars ();
  let e_match = test_parse_expr "typecase_match" "
    match v with
    | x:Int -> x
    | x:Bool -> 1
    | _ -> 0
  " in
  let t_v = test_parse_t "typecase_match" "Int | Bool | String" in
  assert_type_compatible (check [("v", t_v)] e_match) (test_parse_t "typecase_match" "Int") "typecase_match"

let _ =
  reset_tvars ();
  let e_ev = test_parse_prog "ev_constructor_pattern" "
    let rec ev x =
      match x with
      | Add(l, r) -> ev l + ev r
      | i:Int -> i
  " in
  let _ = eval_prog e_ev in
  let rec_t1 = test_parse_t "ev_constructor_pattern" "'rec" in
  let body_t = test_parse_t "ev_constructor_pattern" "Int | Add('rec, 'rec)" in
  (match rec_t1 with 
   | [Var v] -> v.link <- Some body_t 
   | _ -> ());
  let e_ev_apply = test_parse_expr "ev_constructor_pattern_apply" "
  let rec ev x =
      match x with
      | Add(l, r) -> ev l + ev r
      | i:Int -> i
  in ev (Add(Add(1, 2), 3))" in
  assert_type_compatible (check [] e_ev_apply) (test_parse_t "ev_constructor_pattern_apply" "Int") "ev_constructor_pattern_apply"

let _ =
  reset_tvars ();
  let e_const = test_parse_expr "ev_constructor" "Add(Add(1, 2), 3)" in
  let rec_e = test_parse_t "ev_constructor" "'rec" in
  let body_e = test_parse_t "ev_constructor" "Int | Add('rec, 'rec)" in
  (match rec_e with 
   | [Var v] -> v.link <- Some body_e 
   | _ -> ());
  assert_true (subtype [] (check [] e_const) rec_e) "ev_constructor"

let _ =
  reset_tvars ();
  let e_nested = test_parse_expr "nested_constructor_pattern" "fun x -> match x with | Add(i:Int, r) -> i + 1 | _ -> 0" in
  assert_type_compatible (check [] e_nested) (test_parse_t "nested_constructor_pattern" "Add(Int, 'a) -> Int") "nested_constructor_pattern"

let _ =
  reset_tvars ();
  let e_deep = test_parse_expr "deep_nested_constructor_pattern" "
    let rec ev x =
      match x with
      | Add(Add(l1, r1), r) -> ev l1 + ev r1 + ev r
      | i:Int -> i
    in ev
  " in
  let t3 = test_parse_t "deep_nested_constructor_pattern" "'rec" in
  let body_t3 = test_parse_t "deep_nested_constructor_pattern" "Add('rec, 'rec) | Int" in
  (match t3 with 
   | [Var v] -> v.link <- Some body_t3 
   | _ -> ());
  assert_true (subtype [] [Arrow (t3, [Int])] (check [] e_deep)) "deep_nested_constructor_pattern"

let _ =
  print_endline "All tests passed successfully!"
