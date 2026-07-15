open Row_union
open Ast
open Typing
(* テスト *)

let assert_true b msg = if not b then failwith ("Assertion failed: " ^ msg)

let assert_type_eq t1 t2 msg = 
  if not (alpha_equal t1 t2) then (
    Printf.printf "\n[FAIL] %s\n" msg;
    Printf.printf "  Inferred : %s\n" (show_t t1);
    Printf.printf "  Expected : %s\n\n" (show_t t2);
    failwith ("Type mismatch: " ^ msg)
  )

let _ =
  print_endline "Running type system tests..."
let _ =
  assert_true (subtype [] [Int; Bool] [Bool; Int]) "sub_union";
  assert_true (subtype [] [Int; Bool] [Int; Bool; String]) "sub_union2";
  assert_true (not (subtype [] [Int; Bool; String] [Int; Bool])) "sub_union_fail";
  assert_true (subtype [] [Arrow ([Int; Bool], [Int])] [Arrow ([Int], [Int; Bool])]) "sub_arrow"
let _ =
  let e_lam = App (Lam ("x", Add (VarExpr "x", IntExpr 1)), IntExpr 1) in
  assert_type_eq (check [] e_lam) [Int] "lam_apply"
let _ =
  let e_match = Match (VarExpr "v", [(PAnn ("x", [Int]), VarExpr "x"); (PAnn ("x", [Bool]), IntExpr 1); (PDefault, IntExpr 0)]) in
  assert_type_eq (check [("v", [Int; Bool; String])] e_match) [Int] "typecase_match"
let _ =
  let e_ev = Fix ("ev", Lam ("x", Match (VarExpr "x", [
    (PAnn ("i", [Int]), VarExpr "i");
    (PConst ("add", [PVar "l"; PVar "r"]), Add (App (VarExpr "ev", VarExpr "l"), App (VarExpr "ev", VarExpr "r")))
  ]))) in
  let t_ev = check [] e_ev in
  let rec_t1 = [Var (new_var ())] in
  (match rec_t1 with [Var v] -> v.link <- Some [Int; Const ("add", [rec_t1; rec_t1])] | _ -> ());
  assert_type_eq t_ev [Arrow (rec_t1, [Int])] "ev_constructor_pattern";
  let e_ev_apply = App (e_ev, ConstApp ("add", [ConstApp ("add", [IntExpr 1; IntExpr 2]); IntExpr 3])) in
  assert_type_eq (check [] e_ev_apply) [Int] "ev_constructor_pattern_apply"
let _ =
  let e_const = ConstApp ("add", [ConstApp ("add", [IntExpr 1; IntExpr 2]); IntExpr 3]) in
  let rec_e = [Var (new_var ())] in
  (match rec_e with [Var v] -> v.link <- Some [Int; Const ("add", [rec_e; rec_e])] | _ -> ());
  assert_true (subtype [] (check [] e_const) rec_e) "ev_constructor"
let _ =
  let e_nested = Lam ("x", Match (VarExpr "x", [(PConst ("add", [PAnn ("i", [Int]); PVar "r"]), Add (VarExpr "i", IntExpr 1)); (PDefault, IntExpr 0)])) in
  assert_type_eq (check [] e_nested) [Arrow ([Const ("add", [[Int]; [Var (new_var ())] ])], [Int])] "nested_constructor_pattern"
let _ =
  let e_deep = Fix ("ev", Lam ("x", Match (VarExpr "x", [
    (PConst ("add", [PConst ("add", [PVar "l1"; PVar "r1"]); PVar "r"]),
     Add (Add (App (VarExpr "ev", VarExpr "l1"), App (VarExpr "ev", VarExpr "r1")), App (VarExpr "ev", VarExpr "r")));
    (PAnn ("i", [Int]), VarExpr "i")
  ]))) in
  let t3 = [Var (new_var ())] in
  (match t3 with [Var v] -> v.link <- Some [Const ("add", [t3; t3]); Int] | _ -> ());
  assert_true (subtype [] [Arrow (t3, [Int])] (check [] e_deep)) "deep_nested_constructor_pattern"
let _ =
  print_endline "All tests passed successfully!"
